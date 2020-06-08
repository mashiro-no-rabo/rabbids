use anyhow::{bail, Context, Result};
use log::trace;
use std::process::Command;

pub fn ensure_running() -> Result<()> {
  trace!("testing docker");
  let docker_output = Command::new("docker")
    .args(&["version"])
    .output()
    .context("failed to run `docker version`")?;
  if !docker_output.status.success() {
    bail!("Docker is not fully running.");
  }
  trace!("docker is running");

  Ok(())
}

pub fn pull_image(image: &str) -> Result<()> {
  trace!("pulling container image {}", image);
  Command::new("docker")
    .args(&["pull", image])
    .output()
    .context(format!("failed to pull {}", image))?;

  Ok(())
}
