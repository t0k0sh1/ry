use ry::{run_file, run_repl};

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() == 1 {
        // REPL mode
        run_repl();
    } else {
        // File execution mode
        let file_path = &args[1];
        if let Err(e) = run_file(file_path) {
            eprintln!("Error: {}", e);
            std::process::exit(1);
        }
    }
}
