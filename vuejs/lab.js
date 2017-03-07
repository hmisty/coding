var app = new Vue({
  el: '#app',
  data: {
    message: 'Hello world!'
  }
});

Vue.component('todo-item', {
  props: ['todo'],
  template: '<li>{{ todo.text }}</li>'
});

var app7 = new Vue({
  el: '#app-7',
  data: {
    theList: [
      { text: 'apple' },
      { text: 'pear' },
      { text: 'peach' }
    ]
  }
});

