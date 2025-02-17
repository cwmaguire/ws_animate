"use string";

// Create WebSocket connection.
var socket;
var {channel, animator} = qsParams();
setup_divs();
setup_websocket(channel);
window.opener.console.log(`Channel: >${channel}<, animator: >${animator}<`);

function setup_divs(){
  const controlDiv = document.createElement('div');
  controlDiv.id = 'control_div';
  document.body.appendChild(controlDiv);
  setup_controls();

  const canvasDiv = document.createElement('div');
  convasDiv = 'canvas_div';
  document.body.appendChild(canvasDiv);
  const canvas = document.createElement('canvas');
  canvas.id = 'canvas1';
  canvas.width = 400;
  canvas.height = 350;
  canvas.style.border = '1px solid black';
  const ctx = canvas.getContext('2d');
  ctx.scale(0.5, 0.5);
  window.opener.console.log(`Animator ${animator} 2d context tranform:`);
  window.opener.console.dir(ctx.getTransform());
  setup_canvas(canvas);
  canvasDiv.appendChild(canvas);
}

function setup_controls(){
  button('start');
  button('stop');
  br();
  button('freeze');
  button('unfreeze');
  br();
  timing_label();
}

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
    socket.send("channel sub info");
    socket.send("channel sub control");
    socket.send("channel sub draw");
  }
}

function socketMessage(event){
  const obj = JSON.parse(event.data);
  const {name: commandAnimator, type, cmd} = obj;
  if(animator == commandAnimator || cmd == 'finish' || cmd == 'clear'){
    switch(type){
      case 'draw':
        buffer_command(obj);
        break;
      case 'control':
        window.opener.console.log(`animator ${animator} control msg:`);
        window.opener.console.dir(obj);
        console.dir(obj);
        control(obj);
        break;
      case 'info':
        window.opener.console.log(`animator ${animator} info msg:`);
        window.opener.console.dir(obj);
        console.dir(obj);
        info(obj);
        break;
    }
  }else if(!('avg_frame_time' in obj)){
    window.opener.console.log(`animator ${animator} filtered out command:`);
    window.opener.console.dir(obj);
  }
}

function socketError(event){
  console.log("Websocket error: ", event);
}

function socketClose(event){
  console.log("Websocket close. Code: ", event.code, ". Reason: ", event.reason, ". Clean? ", event.wasClean);
}

function log(str){
  console.log(str);
}

function br(){
  const br = document.createElement('br');
  document.querySelector('#control_div').appendChild(br);
}

function button(action){
  const button = document.createElement("input");
  button.type = "button";
  button.id = `${animator}_${action}`;
  button.value = action;
  const command = `animator ${action} ${animator}`;
  button.addEventListener("click", ({data}) => {send(`${command}`)});
  document.querySelector('#control_div').appendChild(button);
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
  document.querySelector('#control_div').appendChild(l);
  document.querySelector('#control_div').appendChild(t);
  document.querySelector('#control_div').appendChild(br);
}

function control(Command){
  const {cmd} = Command;
  //console.log(`Command.cmd ${Command.cmd}`);
  //console.log(`cmd is '${cmd}', ${typeof cmd}`);
  switch(cmd){
    case "clear":
      clear_controls();
      break;
    case "select":
      select(Command);
      break;
    case "textbox":
    case "color":
    case "checkbox":
      input(Command);
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

function clear_controls(){
  remove_all_children(document.querySelector('#control_div'));
}

function remove_all_children(e){
  while(e.firstChild){
    e.removeChild(e.firstChild);
  }
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

  document.querySelector('#control_div').appendChild(l);
  document.querySelector('#control_div').appendChild(s);
  document.querySelector('#control_div').appendChild(br);
}

function input(object){
  const i = document.createElement("input");
  i.setAttribute("type", object.cmd);
  i.id = object.id;
  i.name = object.id;
  i.value = object.value;
  i.checked = object.is_checked;
  const l = document.createElement('label');
  l.textContent = object.label;
  l.htmlFor = i.id;

  const command = `animator set ${object.name} ${object.field}`;
  let eventHandler;
  if(object.cmd == 'checkbox'){
    eventHandler = () => send(`${command} ${i.checked}`);
  }else{
    eventHandler = () => send(`${command} ${i.value}`);
  }
  i.addEventListener("change", eventHandler);

  const br = document.createElement('br');

  document.querySelector('#control_div').appendChild(l);
  document.querySelector('#control_div').appendChild(i);
  document.querySelector('#control_div').appendChild(br);
}

function send(text){
  socket.send(text);
}
