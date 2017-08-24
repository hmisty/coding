/* usage: swift StudyEventProcessing.swift
 */
import Foundation;

/**************************************************/
class ViewHelperSingleton { 
  var btn: Button!;

  static let _instance = ViewHelperSingleton();

  private init() {
    btn = Button();
    btn.addOnClickTarget(self, action: #selector(ViewHelperSingleton.btnClicked));
  }

  @objc func btnClicked() {
    print("ViewHelperSingleton: button is clicked");
  }
}

class ViewHelper {
  var btn: Button!;

  init() {
    btn = Button();
    btn.addOnClickTarget(self, action: #selector(ViewHelper.btnClicked));
  }

  @objc func btnClicked() {
    print("ViewHelper: button is clicked");
  }
}

class View {
  func handleClickEvent() {
    print("View is handling a click event");
  }
}

class Button: View {
  var onClickTarget: Any!;
  var onClickAction: Selector!;

  func addOnClickTarget(_ target: Any?, action: Selector) {
    onClickTarget = target;
    onClickAction = action;
  }

  override func handleClickEvent() {
    print("Button is handling a click event");

    //performSelector -- explicitly marked unavailable
    //Timer.scheduledTimerWithTimeInterval -- explicitly marked unavailable
    let timer = Timer.scheduledTimer(timeInterval: 0.1, target: onClickTarget, selector: onClickAction, userInfo: nil, repeats: false);
    timer.fire();
  }
}

class RootUI {
  var subViews: [View?] = [];

  func addSubView(_ view: View) {
    subViews.append(view);
  }

  init() {
    let v = ViewHelper();
    addSubView(v.btn!);

    let vs = ViewHelperSingleton._instance;
    addSubView(vs.btn!);
  }

  func sendClickEvent(_ eventSource: View) {
    eventSource.handleClickEvent();
  }

  func chainClickEvent() {
    for subview in subViews {
      sendClickEvent(subview!);
    }
  }
}

/**************************************************/
let ui = RootUI();
ui.chainClickEvent();

