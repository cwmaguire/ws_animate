"use string";

// Create WebSocket connection.
var socket;
var {channel, animator} = qsParams();
setup_websocket(channel);
window.opener.console.log(`Channel: >${channel}<, animator: >${animator}<`);

function qsParams() {
    // window.location.search: Sets or returns the querystring part of a URL
    const params = new URLSearchParams(window.location.search);
    return {channel: params.get('channel'), animator: params.get('animator')};
}

function setup_websocket(channel){
  socket = new WebSocket("ws://localhost:8081/ws");
  socket.addEventListener("open", socketOpenListener(channel));
  socket.addEventListener("message", socketMessage);
  socket.addEventListener("error", socketError);
  socket.addEventListener("close", socketClose);
}

function socketOpenListener(channel){
  return (event) => {
    console.log("Opened socket, sending commands");
    socket.send(`channel join ${channel}`);
    socket.send("channel sub log");
    socket.send("channel sub control");
  }
}

function socketMessage(event){
  log(event.data);
  obj = JSON.parse(event.data);
  if(obj.type == "control" && obj.animator == animator){
    control(obj);
  }
}

function socketError(event){
  console.log("Websocket error: ", event);
}

function socketClose(event){
  console.log("Websocket close. Code: ", event.code, ". Reason: ", event.reason, ". Clean? ", event.wasClean);
}

function log(str){
  const d = document.createElement("div");
  d.innerText = str;
  document.body.appendChild(d);
}

function control(Command){
  const {cmd} = Command;
  //console.log(`Command.cmd ${Command.cmd}`);
  //console.log(`cmd is '${cmd}', ${typeof cmd}`);
  switch(cmd){
    case "clear":
      clear();
      break;
    case "select":
      select(Command);
      break;
    case "textbox":
      textbox(Command);
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

  options.forEach(({value, text}) => {
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

function textbox(textbox){
  const t = document.createElement("input");
  t.setAttribute("type", "text");
  t.id = textbox.id;
  t.name = textbox.name;
  t.value = textbox.value;
  const l = document.createElement('label');
  l.textContent = textbox.label;
  l.htmlFor = t.id;

  const command = `animator set ${textbox.animator} ${textbox.field}`;
  t.addEventListener("change", ({data}) => {send(`${command} ${t.value}`)});

  document.body.appendChild(l);
  document.body.appendChild(t);
}

function send(text){
  socket.send(text);
}
