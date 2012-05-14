// xmlhttp handling
MAX_WORKERS = 4

if (window.XMLHttpRequest)
{// code for IE7+, Firefox, Chrome, Opera, Safari
    xmlhttp = function () { return new XMLHttpRequest(); }
}
else
{// code for IE6, IE5
    xmlhttp = function() {return new ActiveXObject("Microsoft.XMLHTTP");}
}

_workers = [];
_slouchers = [];
_tasks = [];
_cache = {};

window["manage_task"] = function (task) {
    if (task) {
        var a;
        if (task.hash) {
            a = _cache[task.hash];
        } else {
            a = null;
        }
        if (a) {
            task.cb(a);
        } else {
            _tasks.push(task);
        }
    }
    if (_tasks.length && (_slouchers.length || _workers.length < MAX_WORKERS)) {
        task = _tasks.shift();
        var worker = _slouchers.pop();
        if (!worker) {
            worker = xmlhttp();
            _workers.push(worker);
        }
        worker.open(task.method, task.path, true);
        worker.onreadystatechange = function () {
            if (worker.readyState == 4) {
                if (worker.status != 200) {
                    if (task.error_cb) { task.error_cb(worker); }
                } else {
                    if (task.hash) {
                        _cache[task.hash] = worker.responseText;
                    }
                    task.cb(worker.responseText);
                }
                worker.abort();
                _slouchers.push(worker);
                manage_task();
            }
        };
        worker.send(task.body);
    }
}

window["abort_all"] = function () {
    for (var i = 0; i < _workers.length; i++) {
        _workers[i].abort();
    }
    _tasks = [];
    _slouchers = [];
    for (i = 0; i < _workers.length; i++) {
        _slouchers.push(_workers[i]);
    }
}

function count(where, what) {
    var r = new RegExp(what, "gm");
    return (where.length - where.replace(r, '').length) / what.length;
}

function get_file_cb (where1, where2, lines) {
    return function(text) {
        where1.innerHTML = text;
        if (where2) { where2.innerHTML = text; }
        if (lines) {
            where1.setAttribute(lines, count(text, '\n') + 1);
        }
    };
}

window["get_file"] = function (what, where1, where2, lines) {
    var path = document.location.pathname;
    path = path.substr(0, path.lastIndexOf('/') + 1) + what;
    manage_task({ method: 'GET',
                  path: path,
                  hash: what,
                  cb: get_file_cb(where1, where2, lines),
                  error_cb: function() {
                      where1.innerHTML = "problem loading " + what;
                  }
                });
}

_loaded = { "examples_dir.js" : true };

window["load_js"] = function (what) {
    if (_loaded[what]) return;
    var path = document.location.pathname;
    path = path.substr(0, path.lastIndexOf('/') + 1) + "compiled-" + what;
    manage_task({ method: 'GET',
                  path: path,
                  cb: function (text) {
                      eval(text);
                  }
                });
}

load_examples();