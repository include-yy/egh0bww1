// https://gist.github.com/cho45/2050199
window._XMLHttpRequest = XMLHttpRequest;
window.XMLHttpRequest = function () {
    this.onreadystatechange = function () {};
    this.readyState = 0 /* UNSENT */;
    this.responseText = null;
    this.responseXML  = null;
    this.status       = 0;
    this.statusText   = '';
};
window.XMLHttpRequest.prototype = {
    abort : function () {
    },

    getAllResponseHeaders : function () {
    },

    open : function (method, url, async, user, pass) {
	this._method = method;
	this._url    = url;
	this._async  = async;
	this.readyState = 1 /* OPENED */;
	this.onreadystatechange();
    },
    send : function (data) {
	var self = this;

	try {
	    self.status = 200;
	    window.XMLHttpRequest.HANDLERS[self._url](this, data);
	} catch (e) {
	    console.log(String(e));
	    self.status = 500;
	    self.responseText = String(e);
	}

	var done = function () {
	    self.readyState = 2 /* HEADERS_RECEIVED */;
	    self.onreadystatechange();

	    self.readyState = 3 /* LOADING */;
	    self.onreadystatechange();

	    self.readyState = 4 /* DONE */;
	    self.onreadystatechange();
	};

	if (self._async) {
	    setTimeout(done, this.wait || 10);
	} else {
	    done();
	}
    },
    setResponseHeader : function () {
    }
};
window.XMLHttpRequest.HANDLERS = {};
window.XMLHttpRequest.HANDLERS.Utils = {
    parseBody : function (data) {
	var ret = {};
	data = data.split(/&/);
	for (var i = 0, len = data.length; i < len; i++) {
	    var kv = data[i].split(/=/);
	    if (!ret[kv[0]]) ret[kv[0]] = [];
	    ret[kv[0]].push(kv[1]);
	}
	return ret;
    }
};
