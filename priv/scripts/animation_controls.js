"use string";

// Create WebSocket connection.
var socket;
const channel = getQueryStringChannel();
setup_websocket(channel);

function getQueryStringChannel() {
    // window.location.search: Sets or returns the querystring part of a URL
    const params = new URLSearchParams(window.location.search);
    return params.get('channel');
}

function setup_websocket(channel){
  socket = new WebSocket("ws://localhost:8081/ws");
  socket.addEventListener("open", socketOpenListener(channel);
  socket.addEventListener("message", socketMessage);
}

function socketOpenListener(channel){
  (event) => {
    socket.send(`channel join ${channel}`);
    socket.send("channel sub control");
  }
}

function socketMessage(event){
  obj = JSON.parse(event.data);
  if(obj.type == "control"){
    control(obj);
  }
}

function control(Command){
  const {cmd} = Command;
  //console.log(`Command.cmd ${Command.cmd}`);
  //console.log(`cmd is '${cmd}', ${typeof cmd}`);
  c = ctx();
  switch(cmd){
    case "clear":
      clear(c);
      break;
    case "select":
      select(c, Command);
      break;
    default:
      console.log(`Ignoring command ${cmd}`);
  }
}

function clear(){
  var body = document.createElement('body');
  document.body = body;
}

function select({id, name, label, options}){

  const s = document.createElement('select');
  s.id = id;
  s.name = name;

  options.forEach(({value, text})) => {
    const o = document.createElement('option');
    o.value = value;
    o.textContent = text;
    s.appendChild(o);
  });

  const l = document.createElement('label');
  l.textContent = label;
  l.htmlFor = id;

  document.body.appendChild(l);
  document.body.appendChild(s);
}

