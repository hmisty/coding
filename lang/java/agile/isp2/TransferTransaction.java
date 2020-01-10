class TransferTransaction extends Transaction {
  UI ui = new UI();

  public void execute() {
    ui.requestTransferAmount();
    Log.l("transfer");
  }
}
