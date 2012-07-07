Ember.VIEW_PRESERVES_CONTEXT = true;
Ember.CP_DEFAULT_CACHEABLE = true;

var Mchat = Em.Application.create({
 ready: function() {
  if (!'WebSocket' in window) {
    alert('Browser does not support websockets.');
  }
  Mchat.initBullet();
  Mchat.sidebarView.append();
  Mchat.chatboxesView.append();
 }   
});

// Statemanager
Mchat.stateManager = Em.StateManager.create({
  initialState: 'loggedOut',

  loggedOut: Em.State.create({
    enter: function() {
    },
    exit: function() {
    },
    login: function(manager, context) {
      Mchat.api.login(context.username);
    },
    loginResult: function(manager, context) {
      if (context.success) {
        manager.goToState('loggedIn');
      } else {
        alert('Username already in use.');
      }
    }
  }),

  loggedIn: Em.State.create({
    enter: function() {
      Mchat.api.getUsers();
    },
    exit: function() {
      // TODO Maybe clean up or reset certain ui
    },
    disconnect: function(manager, context) {
      manager.goToState('loggedOut');
    }
  })
});

// Views
Mchat.sidebarView = Em.View.create({
  templateName: 'sidebar-view',
  classNames: 'sidebar-view'
});

Mchat.chatboxesView = Em.View.create({
  templateName: 'chatboxes-view',
  className: 'chatboxes-view'
});

Mchat.LoginView = Em.View.extend({
  templateName: 'login-view',
  classNames: ['login-view'],
  username: '',
  submit: function(e) {
    var username = this.get('username');
    if (Em.empty(username)) {
      alert("Username cannot be empty.");
    } else {
      Mchat.stateManager.send('login', {username: username});
    }
    return false;
  }
});

Mchat.CurrentUserView = Em.View.extend({
});

Mchat.UsersCollectionView = Em.CollectionView.extend({
  content: ''
});

// Controllers
Mchat.usersController = Em.ArrayController.create();

// Models
Mchat.User = Em.Object.extend({
  username: null,
  status: 'offline'
});
Mchat.currentUser = Mchat.User.create();

// JSONRPC via Bullet
Mchat.Bullet = null;

Mchat.JsonRPCSend = function(json) {
  json.jsonrpc = '2.0';
  Mchat.Bullet.send(JSON.stringify(json));
};

Mchat.initBullet = function() {
  // TODO Make configurable via server
  Mchat.Bullet = $.bullet('ws://localhost:8080/mchat-api');
  Mchat.Bullet.onopen = function() {
    console.log('Main websocket: opened');
  };
  
  Mchat.Bullet.onclose = function() {
    console.log('Main websocket: closed');
  };

  Mchat.Bullet.onmessage = function(e) {
    if (e.data instanceof ArrayBuffer) {
      console.log('Main websocket: error - got binary data?');
    } else {
      var resp = JSON.parse(e.data);
      if (resp.error === undefined) {
        var method = resp.id;
        var result = resp.result;
        window['Mchat']['api'][method](result);
      } else {
        window.alert(resp.error.code + ': ' + resp.message);
      }
    }
  };

  Mchat.Bullet.onheartbeat = function() {
    Mchat.JsonRPCSend({method: 'ping'});
  };
};

// Mchat server api via jsonrpc2
Mchat.api = Em.Object.create({
  login: function(username) {
    var req = {
      method: 'login', id: '_login',
      params: {username: username}
    };
    Mchat.JsonRPCSend(req);
  },
  _login: function(result) {
    Mchat.stateManager.send('loginResult', result);
  },

  getUsers: function() {
    var req = {method: 'getUsers', id: '_getUsers'};
    Mchat.JsonRPCSend(req);
  },
  _getUsers: function(result) {
    var arr = result.map(function(item, index, self) {
      var user = Mchat.User.create();
      user.setProperties(item);
      return user;
    });
    Mchat.usersController.set('content', arr);
  }
});

