public class StudyEventProcessing {
  public static void main(String[] args) {
    Button a = new Button();
    EventListener b = new EventListener();
    a.addOnClickListener(b);
    a.click();
  }
}

interface View {
  public String toString();
}

interface OnClickListener {
  public void onClick(View eventSource);
}

interface OnDoubleClickListener {
  public void onDoubleClick(View eventSource);
}

class Button implements View {
  OnClickListener l;

  public String toString() {
    return "button";
  }

  public void addOnClickListener(EventListener l) {
    this.l = l;
  }

  public void click() {
    if (l != null) {
      l.onClick(this);
    }
  }
}

class EventListener implements OnClickListener, OnDoubleClickListener {
  public void onClick(View eventSource) {
    System.out.println("event: click; source: " + eventSource);
  }

  public void onDoubleClick(View eventSource) {
    System.out.println("event: double click; source: " + eventSource);
  }
}

