Ember.VIEW_PRESERVES_CONTEXT = true;
Ember.CP_DEFAULT_CACHEABLE = true;

var Mchat = Em.Application.create({
 ready: function() {
  if (!'WebSocket' in window) {
    alert('Browser does not support websockets.');
  }
  $.getJSON('/client-config.json', function(data) {
    Mchat.IP = data.ip,
    Mchat.PORT = data.port,
    Mchat.initBullet();
    Mchat.sidebarView.append();
  });
 }
});

Mchat.IP = "localhost";
Mchat.PORT = "8080";

// Statemanager
Mchat.stateManager = Em.StateManager.create({
  initialState: 'loggedOut',

  loggedOut: Em.State.create({
    enter: function() {
    },
    exit: function() {
      Mchat.sidebarView.get('childViews')[1].toggle();
      Mchat.sidebarView.get('childViews')[0].toggle();
      Mchat.chatboxesView.append();
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
      Mchat.sidebarView.get('childViews')[0].toggle();
      Mchat.sidebarView.get('childViews')[1].toggle();
      Mchat.usersController.set('content', '');
      Mchat.currentUser.set('username', '');
      Mchat.chatboxesView.remove();
      // TODO force reload hack to prevent certain bugs
      // related to zombie workers, states and views
      // remove if bugs are cleared
      window.location.reload(); 
    },
    disconnect: function(manager, context) {
      Mchat.api.logout();
      manager.goToState('loggedOut');
    }
  })
});

// Views
Mchat.sidebarView = Em.View.create({
  templateName: 'sidebar-view',
  classNames: ['sidebar-view']
});

Mchat.FileSystemView = Em.View.extend({
  tagName: 'iframe',
  attributeBindings: ['src'],
  src: 'filesystem:http://'+
    Mchat.IP+':'+Mchat.PORT+'/temporary/'
});

Mchat.chatboxesView = Em.ContainerView.create({
  classNames: ['chatboxes-view']
});

Mchat.LoginView = Em.View.extend({
  templateName: 'login-view',
  classNames: ['login-view'],
  username: '',
  toggle: function() {
    this.$().toggle();
  },
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
  templateName: 'current-user-view',
  usernameBinding: 'Mchat.currentUser.username',
  classNames: ['current-user-view'],
  toggle: function() {
    this.$().toggle();
  },
  submit: function(e) {
    Mchat.stateManager.send('disconnect');
    return false;
  }
});

Mchat.UsersCollectionView = Em.CollectionView.extend({
  tagName: 'ul',
  classNames: ['unstyled', 'users-collection-view'],
  contentBinding: 'Mchat.usersController.content',
  itemViewClass: Em.View.extend({
    template: Em.Handlebars.compile(
      '<div class="circle"></div>{{content.username}} \
      <i class="pull-right icon-user"></i>'),
    click: function(e) {
      var username = this.content.username;
      if (username === Mchat.currentUser.get('username'))
        return false;
      var childViews = Mchat.chatboxesView.get('childViews');
      var view = childViews.filterProperty('to', username);
      if (Em.empty(view)) {
        var view = Mchat.ChatBoxView.create({to: username});
        childViews.pushObject(view);
      }
      return false;
    }
  })
});

Mchat.ChatBoxView = Em.View.extend({
  templateName: 'chatbox-view',
  classNames: ['chatbox-view'],
  to: '',
  msg: '',
  append: function(username, msg) {
    this.$('.chat-log').append(
      "<div><span><b>" + username + ": </b></span>" + 
      "<span>" + msg + "</span></div>"
    ).prop('scrollTop', $('.chat-log').prop('scrollHeight'));
  },
  keyUp: function(e) {
    if (e.keyCode === 13) {
      var to = this.get('to');
      var msg = this.get('msg');
      Mchat.api.sendMsg(to, msg);
      this.append(Mchat.currentUser.get('username'), msg);
      this.set('msg', '');
    }
    return false;
  },
  close: function(e) {
    Mchat.chatboxesView.get('childViews').removeObject(this);
    return false;
  },
  send: function(e) {
    this.get('childViews')[0].toggle();
  }
});

Mchat.UploaderView = Em.View.extend({
  templateName: 'upload-view',
  classNames: ['upload-view'],
  uploader: null,
  toggle: function() {
    this.$().toggle();
  },
  send: function(e) {
    var file = this.$('.send-file').prop('files')[0];
    var username = this.getPath('parentView.to');
    var worker = this.uploader = new Worker('js/uploader.js');
    var $progressBar = this.$('div.bar');

    worker.addEventListener('message', function(e) {
      var status = e.data.status;
      if (status === 'progress') {
        $progressBar.css('width', e.data.progress + '%');
      } else if (status === 'opened') {
        console.log('Upload websocket: opened.');
        Mchat.api.sendFileRequest(username, file.name,
                                  file.type, file.size);
      } else if (status === 'closed') {
        console.log('Upload websocket: closed.');
      } else if (status === 'completed') {
        console.log('Upload websocket: completed transfer.');
      }
    }, false);
    worker.postMessage({cmd: 'init',
                        file: file,
                        username: username,
                        ip: Mchat.IP,
                        port: Mchat.PORT});
    return false;
  },
  sendFile: function(answer, pid) {
    if (answer) {
      this.uploader.postMessage({cmd: 'start', pid: pid});
    } else {
      this.uploader = null;
      alert('Send File Request: Not accepted.');
    }
  }
});

Mchat.DownloadView = Em.View.extend({
  templateName: 'download-view',
  classNames: ['download-view'],
  fileName: '',
  fileType: '',
  fileSize: '',
  downloader: null,
  close: function() {
    this.$().toggle();
    return false;
  },
  yes: function(e) {
    var to = this.getPath('parentView.to');
    var worker = this.downloader = new Worker('js/downloader.js');
    var $progressBar = this.$('div.bar');
    var $close = this.$('button.hide');
    var $cancel = this.$('button.cancel');
    var file = {name: this.fileName,
                type: this.fileType,
                size: this.fileSize};
    this.$('button.yes').hide();
    this.$('button.no').hide();
    this.$('button.hide').hide();
    this.$('button.cancel').show();
    worker.addEventListener('message', function(e) {
      var status = e.data.status;
      if (status === 'progress') {
        $progressBar.css('width', e.data.progress + '%');
      } else if (status === 'opened') {
        console.log('Download websocket: opened.');
        worker.postMessage({cmd: 'start', file: file});
      } else if (status === 'download') {
        Mchat.api.sendFileReply(to, true, e.data.pid);
      } else if (status === 'closed') {
        console.log('Download websocket: closed.');
      } else if (status === 'completed') {
        console.log('Download websocket: completed');
        $close.show();
        $cancel.hide();
        $('iframe').attr('src', function(i, val) {return val;});
      }
    }, false);
    worker.postMessage({cmd: 'init',
                        ip: Mchat.IP, 
                        port: Mchat.PORT});
    return false;
  },
  no: function(e) {
    var to = this.getPath('parentView.to');
    Mchat.api.sendFileReply(to, false, "");
    this.$().hide();
    return false;
  },
  cancel: function(e) {
    this.downloader.terminate();
    this.$().hide();
    return false;
  },
  sendFileRequest: function(result) {
    this.set('fileName', result.fileName);
    this.set('fileType', result.fileType);
    this.set('fileSize', result.fileSize);
    this.$('button.yes').show();
    this.$('button.no').show();
    this.$('button.cancel').hide();
    this.$('button.hide').hide();
    this.$('div.bar').css('width', '0%');
    this.$().show();
  }
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
  Mchat.Bullet = $.bullet('ws://'+Mchat.IP+':'+Mchat.PORT+'/mchat-api');
  Mchat.Bullet.onopen = function() {
    console.log('Main websocket: opened');
  };
  
  Mchat.Bullet.onclose = function() {
    console.log('Main websocket: closed');
    Mchat.stateManager.goToState('loggedOut');
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
        window.alert(resp.error.code + ': ' + resp.error.message);
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
    Mchat.currentUser.setProperties({username: username,
                                     status: 'online'});
    Mchat.JsonRPCSend(req);
  },
  _login: function(result) {
    Mchat.stateManager.send('loginResult', result);
  },

  logout: function() {
    Mchat.JsonRPCSend({method: 'logout'});
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
  },

  _userStatus: function(result) {
    var user = Mchat.usersController.
      findProperty('username', result.username);
    if (result.status === 'offline') {
      if (!Em.empty(user)) {
        Mchat.usersController.removeObject(user);
      }
      this._sendMsg({
        from: result.username,
        msg: '<i>Gone offline.</i>'});
    } else {
      if (Em.empty(user)) {
        var user = Mchat.User.create();
        user.setProperties(result);
        Mchat.usersController.pushObject(user);
      } else {
        user.set('status', result.status);
      }
    }
  },

  sendMsg: function(to, msg) {
    var req = {
      method: 'sendMsg',
      params: {to: to, msg: msg}};
    Mchat.JsonRPCSend(req);
  },
  _sendMsg: function(result) {
    var view = 
      Mchat.chatboxesView.get('childViews').
      filterProperty('to', result.from);
    if (Em.empty(view)) {
      view = Mchat.ChatBoxView.create({to: result.from});
      Mchat.chatboxesView.get('childViews').pushObject(view);
      Em.run.end();
      view.append(result.from, result.msg);
    } else {
      view[0].append(result.from, result.msg);
    }
  },

  // Send File request and expect a send file reply
  sendFileRequest: function(to, fileName, fileType, fileSize) {
    var req = { 
      method: 'sendFileRequest',
      params: {to: to, fileName: fileName, fileType: fileType, fileSize: fileSize}
    };
    Mchat.JsonRPCSend(req);
  },
  _sendFileReply: function(result) {
    var view = Mchat.chatboxesView.
      get('childViews').filterProperty('to', result.from);
    if (Em.empty(view)) {
      // Do nothing ?
    } else {
      view[0].get('childViews').
        filterProperty('templateName', 'upload-view')[0].
        sendFile(result.answer, result.pid);
    }
  },

  // Receive file request and send reply
  _sendFileRequest: function(result) {
    var view = Mchat.chatboxesView.
      get('childViews').filterProperty('to', result.from);
    if (Em.empty(view)) {
      view = Mchat.ChatBoxView.create({to: result.from});
      Mchat.chatboxesView.get('childViews').pushObject(view);
      Em.run.end();
      view.get('childViews').
        filterProperty('templateName', 'download-view')[0].
        sendFileRequest(result);
    } else {
      view[0].get('childViews').
        filterProperty('templateName', 'download-view')[0].
        sendFileRequest(result);
    }
  },
  sendFileReply: function(to, answer, pid) {
    // Pid will be the download worker <--> erlang pid
    var req = {
      method: 'sendFileReply',
      params: {to: to, answer: answer, pid: pid}};
    Mchat.JsonRPCSend(req);
  }
});

