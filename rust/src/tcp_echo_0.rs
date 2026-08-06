use mio;
use slab;
use std::io::Write;


const BUF_LEN: usize = 16 * 1024;
//const BUF_LEN: usize = 4;


const SERVER: mio::Token = mio::Token(usize::MAX);

struct Connection {
    addr: std::net::SocketAddr,
    stream: mio::net::TcpStream,
    buf: std::collections::VecDeque<u8>,
    from_peer_recv_closed: bool,
}

type Connections = slab::Slab<Connection>;

pub fn run()
{
    let mut poll = mio::Poll::new().expect("fail in Poll::new");
    let mut events = mio::Events::with_capacity(64);
    let addr = std::net::SocketAddr::from(([127, 0, 0, 1], 50000));

    let mut listener = mio::net::TcpListener::bind(addr).expect("fail in TcpListener::bind");

    poll.registry().register(&mut listener, SERVER, mio::Interest::READABLE).expect("fail in Poll::register");

    let mut connections: Connections = slab::Slab::new();

    loop {
        println!("polling: conn_size={}", connections.len());

        if let Err(e) = poll.poll(&mut events, None) {
            if e.kind() == std::io::ErrorKind::Interrupted {
                continue;
            }
            eprintln!("fail in Poll::poll: {}", e);
            break;
        }
        
        for ev in events.iter() {
            if ev.token() == SERVER {
                listener_accept_all(&mut connections, &mut listener, &mut poll);
            } else {
                match connections.get_mut(ev.token().0) {
                    Some(conn) => {
                        // Micro-optimisation: try to do as much reading/writing as makes sense without waiting
                        // for another readable/writable event.

                        // if socket is writable, try to write first to free some buffer for reading
                        if ev.is_writable() {
                            let peer_closed_recv = connection_handle_write(conn);
                            if peer_closed_recv {
                                // peer closed its recv connection, no point in reading, remove connection
                                // Possible alternative behaviour: keep reading and discard data
                                // until peer closes its send connection.
                                poll.registry().deregister(&mut conn.stream).expect("fail in Poll::deregister");
                                println!("peer stopped receiving, conn REMOVED: addr={}", conn.addr);
                                connections.remove(ev.token().0);
                                continue;
                            }
                        }
                        if ev.is_readable() {
                            let was_empty_before_reading = conn.buf.is_empty();
                            connection_handle_read(conn);
                            // Trying to write after reading, only makes sense if buffer was empty
                            // before reading. If it was not empty, previous writing was partial,
                            // and most likely socket is still not writable.
                            if was_empty_before_reading && ev.is_writable() {
                                connection_handle_write(conn);
                            }
                        }

                        // update connection
                        let new_interest =
                            if conn.from_peer_recv_closed {
                                if conn.buf.is_empty() {
                                    // peer will not send more data, all current data is sent, we are done.
                                    // remove conneciton
                                    poll.registry().deregister(&mut conn.stream).expect("fail in Poll::deregister");
                                    println!("connection closed R / REMOVED: addr={}", conn.addr);
                                    connections.remove(ev.token().0);
                                    continue;
                                } else {
                                    // peer will not send more data, but we still have data to send
                                    println!("new interest: W_finish_writing, addr={}", conn.addr);
                                    mio::Interest::WRITABLE
                                }
                            } else if conn.buf.len() >= conn.buf.capacity() {
                                // buffer is full, don't want to read more data
                                println!("new interest: W_buf_full, addr={}", conn.addr);
                                mio::Interest::WRITABLE
                            } else if conn.buf.is_empty() {
                                // buffer is empty, nothing to write
                                println!("new interest: R_buf_empty, addr={}", conn.addr);
                                mio::Interest::READABLE
                            } else {
                                println!("new interest: RW, addr={}", conn.addr);
                                mio::Interest::READABLE.add(mio::Interest::WRITABLE)
                            };
                        poll.registry().reregister(&mut conn.stream, ev.token(), new_interest).expect("fail in Poll::reregister");
                    }
                    None => {
                        eprintln!("connection not found for token {:?}", ev.token());
                    }
                }
            }
        }
    }
}


fn listener_accept_all(connections: &mut Connections, listener: &mut mio::net::TcpListener, poll: &mut mio::Poll)
{
    // At least two possible implementations here:
    // 1. Accept all connections in a loop until WouldBlock is returned. This would optimise for
    //    "stream of new connections" case, we accept as many connections as possible before
    //    returning to the event loop.
    // 2. Accept only one connection per event, which means we call accept() only once and don't
    //    call it again, because most likely it will return WouldBlock. If multiple connections are
    //    waiting, we will get another event for the next connection.
    // We implement (2), this is more likely scenario in real world.
    match listener.accept() {
        Ok((mut stream, addr)) => {
            let entry = connections.vacant_entry();
            poll.registry().register(&mut stream, mio::Token(entry.key()), mio::Interest::READABLE).expect("fail in Poll::register");
            entry.insert(Connection {
                addr,
                stream,
                buf: std::collections::VecDeque::with_capacity(BUF_LEN),
                from_peer_recv_closed: false,
            });
            println!("accepted new connection from {}", addr);
        }
        //Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
        //    // not possible if we accept only one connection
        //    return;
        //}
        Err(e) => {
            // assert that error kind is not WouldBlock, because we had "readable" event.
            assert!(e.kind() != std::io::ErrorKind::WouldBlock);
            eprintln!("fail in TcpListener::accept: {}", e);
            //break;
        }
    }
}


// return whether write was empty (perr closed its recv conneciton)
fn connection_handle_write(connection: &mut Connection) -> bool
{
    //assert!(connection.buf.is_empty());
    if connection.buf.is_empty() {
        println!("handle_write: buf is empty");
        return false;
    }

    let (front, back) = connection.buf.as_slices();
    let chunk = if !front.is_empty() {
        front
    } else {
        back
    };

    match connection.stream.write(chunk) {
        Ok(0) => {
            println!("handle_write: zero write");
            return true;
        }
        Ok(n) => {
            println!("handle_write: some write: n={}", n);
            connection.buf.drain(..n);
        }
        Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
            // do nothing
        }
        Err(e) => {
            eprintln!("fail in TcpStream::write: {}", e);
        }
    }
    return false;
}


fn connection_handle_read(conn: &mut Connection)
{
    assert!(conn.buf.capacity() >= conn.buf.len());
    let free_size = conn.buf.capacity() - conn.buf.len();

    if free_size == 0 {
        return;
    }
    //assert!(free_size > 0); // we don't register "readable" interest if buffer was full.

    assert!(free_size <= BUF_LEN);
    let mut tmp_buf = [0u8; BUF_LEN];

    use std::io::Read;
    match conn.stream.read(&mut tmp_buf[..free_size]) {
        Ok(0) => {
            conn.from_peer_recv_closed = true;
            return;
        },
        Ok(n) => {
            conn.buf.extend(&tmp_buf[..n]);
        }
        Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
            return;
        }
        Err(e) => {
            eprintln!("fail in TcpStream::read: {}", e);
            return;
        }
    }
}


#[cfg(test)]
mod tests {
use super::*;


#[test]
fn test_conn()
{
    assert!(true);
}

#[test]
fn test_2()
{
    assert!(false);
}

} // mod tests

