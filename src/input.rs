use anyhow::Context;
use crossbeam::channel::{unbounded, Receiver};
use crossterm::event::{self, Event, KeyCode, KeyEvent};
use log::trace;
use std::{thread, time::Duration};

pub enum Action {
  NewNode,
  Quit,
}

pub fn setup_input() -> Receiver<Action> {
  let (s, r) = unbounded();
  static POLL_TIMEOUT: Duration = Duration::from_millis(100);

  thread::spawn(move || loop {
    if event::poll(POLL_TIMEOUT)
      .context("failed to poll crossterm event")
      .unwrap()
    {
      let e = event::read()
        .context("failed to read crossterm event even though polled")
        .unwrap();
      trace!("received input event: {:?}", e);

      let action = match e {
        Event::Key(KeyEvent {
          code: KeyCode::Char('n'),
          modifiers: _,
        }) => Some(Action::NewNode),
        Event::Key(KeyEvent {
          code: KeyCode::Char('q'),
          modifiers: _,
        }) => Some(Action::Quit),
        _ => None,
      };
      if let Some(a) = action {
        s.send(a).context("failed to send message to input channel").unwrap();
      }
    }
  });

  r
}
