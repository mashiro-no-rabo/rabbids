use anyhow::{bail, Context, Result};
use crossterm::{
  event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEvent},
  execute,
  terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use log::trace;
use std::{
  io::{stdout, Write},
  process::Command,
  sync::mpsc,
  thread,
  time::{Duration, Instant},
};
use structopt::StructOpt;
use tui::{
  backend::CrosstermBackend,
  layout::{Constraint, Direction, Layout},
  widgets::{Block, Borders},
  Terminal,
};

#[derive(Debug, StructOpt)]
#[structopt(about = env!("CARGO_PKG_DESCRIPTION"))]
struct Opt {
  #[structopt(long, help = "Do a clean startup (remove all resources)")]
  clean: bool,
  #[structopt(long, help = "Consul image (https://www.consul.io/)", default_value = "consul:1.7.3")]
  consul: String,
  #[structopt(
    long,
    help = "gobetween image (http://gobetween.io/)",
    default_value = "yyyar/gobetween:latest"
  )]
  gobetween: String,
  #[structopt(
    long,
    help = "RabbitMQ image (https://www.rabbitmq.com/)",
    default_value = "rabbitmq:3.8-management-alpine"
  )]
  rabbitmq: String,
}

fn main() -> Result<()> {
  pretty_env_logger::init();
  trace!("logger initiated");

  trace!("parsing opts");
  let opt = Opt::from_args();
  trace!("opts parsed #{:?}", &opt);

  // test docker is running
  trace!("testing docker");
  let docker_output = Command::new("docker")
    .args(&["version"])
    .output()
    .context("failed to run `docker version`")?;
  if !docker_output.status.success() {
    bail!("Docker is not fully running.");
  }
  trace!("docker version succeed");

  // setup terminal
  trace!("setting up terminal");
  enable_raw_mode()?;
  let mut stdout = stdout();
  execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
  let backend = CrosstermBackend::new(stdout);
  let mut terminal = Terminal::new(backend)?;
  terminal.hide_cursor()?;
  terminal.clear()?;
  trace!("terminal ready for tui");

  // Setup input handling
  trace!("setting up input handling");
  let (tx, rx) = mpsc::channel();

  thread::spawn(move || loop {
    if event::poll(Duration::from_millis(100)).unwrap() {
      if let Event::Key(key) = event::read().unwrap() {
        tx.send(key).unwrap();
      }
    }
  });
  trace!("input handling ready");

  trace!("starting tui loop");
  loop {
    terminal.draw(|mut f| {
      let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Min(0), Constraint::Length(3)].as_ref())
        .split(f.size());

      let block = Block::default().title("Block").borders(Borders::ALL);
      f.render_widget(block, chunks[0]);
      let block = Block::default().title(" Help ").borders(Borders::ALL);
      f.render_widget(block, chunks[1]);
    })?;

    match rx.recv()? {
      _ => {
        disable_raw_mode()?;
        execute!(terminal.backend_mut(), LeaveAlternateScreen, DisableMouseCapture)?;
        terminal.show_cursor()?;
        break;
      }
    }
  }

  Ok(())
}
