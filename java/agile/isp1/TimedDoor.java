class TimedDoor extends Door implements TimerClient {
  boolean open = false;
  public void lock() {
    open = false;
    Log.l("door closed");
  }
  public void unlock() {
    open = true;
    Log.l("door opened");
  }
  public boolean isDoorOpen() {
    Log.l("check door status: " + (open ? "open":"closed"));
    return open;
  }
  public void timeout() {
    lock();
  }
}
