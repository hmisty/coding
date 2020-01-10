public class Demo {
  public static void main(String[] args) {
    Log.l("ok");
    TimedDoor door = new TimedDoor();
    Timer timer = new Timer();
    timer.register(10, door);
    door.unlock();
    timer.tick();
  }
}
