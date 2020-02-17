#[test]
fn load_test() {
    use emu6502::Emulator;
    env_logger::init();

    let code = include_bytes!("asm/ex01.o65");
    let mut emulator = Emulator::with_rom(code, 0x600);
    emulator.set_pc(0x600);
    
    for _ in 0..30 {
        if let Err(e) = emulator.step() {
            log::error!("{}", e);
        }
    }

    //let a = [1, 2, 3u32];
    //let b = &a[1..];
    //assert_eq!(b.len(), 2);

    //let b = &a[2..];
    //assert_eq!(b.len(), 1);

    //let b = &a[3..];
    //assert_eq!(b.len(), 0);

    //let single_element = [1];
    //let slice_to_nothing = &single_element[1..];
    //assert_eq!(slice_to_nothing.len(), 0);
}
