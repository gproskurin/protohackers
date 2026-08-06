mod tcp_echo_0;

fn main()
{
    let thr_0 = std::thread::spawn(|| {
        tcp_echo_0::run();
    });

    thr_0.join().expect("TCP Echo thread panicked");
}

