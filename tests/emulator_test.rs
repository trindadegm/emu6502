#[test]
fn load_test() {
    use emu6502::Emulator;
    use emu6502::StatusFlag;
    env_logger::init();

    let code = include_bytes!("asm/ex01.o65");
    let mut emulator = Emulator::with_rom(code, 0x600);
    emulator.set_pc(0x600);
    
    // Run several steps (more than enough, can't leave it forever, the limit is to be fail-safe)
    for _ in 0..300 {
        if let Err(e) = emulator.step() {
            log::error!("{}", e);
            break;
        }
        if emulator.is_flag_set(StatusFlag::BreakCommand) {
            break;
        }
    }
    assert_eq!(&emulator.ram()[..20], &[10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);

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
