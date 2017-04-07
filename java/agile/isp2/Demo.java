public class Demo {
  public static void main(String[] args) {
    Log.l("ok");
    Transaction t = new TransferTransaction();
    t.execute();
  }
}
