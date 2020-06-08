use anyhow::Result;
use crossterm::{
  execute,
  terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use log::trace;
use std::{
  io::{stdout, Write},
  time::Duration,
};
use structopt::StructOpt;
use tui::{
  backend::CrosstermBackend,
  layout::{Constraint, Direction, Layout},
  widgets::{Block, Borders},
  Terminal,
};

mod input;
use input::*;
mod docker;

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
  docker::ensure_running()?;

  // pull containers
  docker::pull_image(&opt.consul)?;
  docker::pull_image(&opt.gobetween)?;
  docker::pull_image(&opt.rabbitmq)?;
  trace!("images pulled");

  // setup terminal
  trace!("setting up terminal");
  enable_raw_mode()?;
  let mut stdout = stdout();
  execute!(stdout, EnterAlternateScreen)?;
  let backend = CrosstermBackend::new(stdout);
  let mut terminal = Terminal::new(backend)?;
  terminal.hide_cursor()?;
  terminal.clear()?;
  trace!("terminal ready for tui");

  // Setup input handling
  trace!("setting up input handling");
  let input = setup_input();
  trace!("input handling ready");

  trace!("starting tui loop");
  loop {
    terminal.draw(|mut f| {
      let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Min(0), Constraint::Length(3)].as_ref())
        .split(f.size());

      let block = Block::default().title(" Nodes ").borders(Borders::ALL);
      f.render_widget(block, chunks[0]);
      let block = Block::default().title(" Help ").borders(Borders::ALL);
      f.render_widget(block, chunks[1]);
    })?;

    static RECV_TIMEOUT: Duration = Duration::from_millis(200);

    match input.recv_timeout(RECV_TIMEOUT) {
      Ok(Action::NewNode) => {}
      Ok(Action::Quit) => {
        disable_raw_mode()?;
        execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
        terminal.show_cursor()?;
        break;
      }
      _ => {}
    }
  }

  Ok(())
}
