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
    button('start');
    button('stop');
    br();
    button('freeze');
    button('unfreeze');
    br();
    timing_label();
    socket.send(`channel join ${channel}`);
    //socket.send("channel sub log");
    socket.send("channel sub info");
    socket.send("channel sub control");
  }
}

function socketMessage(event){
  const obj = JSON.parse(event.data);
  const {animator: anim, type} = obj;
  switch(anim + type){
    case animator + 'control':
      control(obj);
      break;
    case animator + 'info':
      info(obj);
      break;
  }
}

function socketError(event){
  console.log("Websocket error: ", event);
}

function socketClose(event){
  console.log("Websocket close. Code: ", event.code, ". Reason: ", event.reason, ". Clean? ", event.wasClean);
}

function log(str){
  //const d = document.createElement("div");
  //d.innerText = str;
  //document.body.appendChild(d);
  console.log(str);
}

function br(){
  const br = document.createElement('br');
  document.body.appendChild(br);
}

function button(action){
  const button = document.createElement("input");
  button.type = "button";
  button.id = `${animator}_${action}`;
  button.value = action;
  const command = `animator ${action} ${animator}`;
  button.addEventListener("click", ({data}) => {send(`${command}`)});
  document.body.appendChild(button);
}

function timing_label(){
  const t = document.createElement("input");
  t.setAttribute("type", "text");
  t.id = 'timing';
  t.name = 'timing';
  t.value = '';
  const l = document.createElement('label');
  l.textContent = 'timing';
  l.htmlFor = t.id;

  const br = document.createElement('br');
  document.body.appendChild(l);
  document.body.appendChild(t);
  document.body.appendChild(br);
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

function info(msg){
  if("avg_frame_time" in msg){
     document.getElementById('timing').value = msg.avg_frame_time;
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

  // TODO handle select event

  const l = document.createElement('label');
  l.textContent = label;
  l.htmlFor = id;

  const br = document.createElement('br');

  document.body.appendChild(l);
  document.body.appendChild(s);
  document.body.appendChild(br);
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

  const br = document.createElement('br');

  document.body.appendChild(l);
  document.body.appendChild(t);
  document.body.appendChild(br);
}

function send(text){
  socket.send(text);
}
