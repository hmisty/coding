class Timer {
  int _timeout;
  TimerClient _client;
  public void register(int timeout, TimerClient client) {
    _timeout = timeout;
    _client = client;
  }
  public void tick() {
    Log.l(_timeout + " passed!");
    _client.timeout();
  }
}
