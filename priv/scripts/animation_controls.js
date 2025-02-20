"use string";

function animation_controls_start(channel,
                                  animator,
                                  element = document.body,
                                  hasCanvas = false){
  const context = {channel, animator, element, hasCanvas};
  setup_divs(context);
  setup_websocket(context);
}

function setup_divs(context){
  const {animator, element} = context;
  const staticControlDiv = document.createElement('div');
  staticControlDiv.id = `${animator}_static_control_div`;
  setup_static_controls(context, staticControlDiv);
  element.appendChild(staticControlDiv);

  const controlDiv = document.createElement('div');
  controlDiv.id = `${animator}_control_div`;
  element.appendChild(controlDiv);
  context.controlDiv = controlDiv;

  if(context.hasCanvas){
    canvas(context);
  }
}

function canvas(context){
  const canvasDiv = document.createElement('div');
  canvasDiv.id = 'canvas_div';
  context.element.appendChild(canvasDiv);

  const canvas = document.createElement('canvas');
  canvas.id = 'canvas1';
  canvas.width = 400;
  canvas.height = 350;
  canvas.style.border = '1px solid black';
  const ctx = canvas.getContext('2d');
  ctx.scale(0.5, 0.5);
  //window.opener.console.log(`Animator ${animator} 2d context tranform:`);
  //window.opener.console.dir(ctx.getTransform());
  setup_canvas(canvas);
  canvasDiv.appendChild(canvas);
}

function setup_static_controls(context){
  const {animator, element} = context;
  const add = (e) => {element.appendChild(e)};
  const br = () => document.createElement('br');

  add(controls_button(context, 'start'));
  add(controls_button(context, 'stop'));
  add(br());
  add(controls_button(context, 'freeze'));
  add(controls_button(context, 'unfreeze'));
  add(br());
  add(timing_label(context));
}

function setup_websocket(context){
  const {element} = context;
  const socket = new WebSocket("ws://localhost:8081/ws");
  context.socket = socket;
  socket.addEventListener("open", socket_open_listener(context));
  socket.addEventListener("message", socket_message_listener(context));
  socket.addEventListener("error", socket_error);
  socket.addEventListener("close", socket_close);

  function socket_error(event){
    console.log("Websocket error: ", event);
  }

  function socket_close(event){
    console.log("Websocket close. Code: ", event.code, ". Reason: ", event.reason, ". Clean? ", event.wasClean);
  }

}

function socket_open_listener({socket, channel}){
  return (event) => {
    console.log("Opened socket, sending commands");
    socket.send(`channel join ${channel}`);
    socket.send("channel sub info");
    socket.send("channel sub control");
    socket.send("channel sub draw");
  };
}

function socket_message_listener(context){
  const {animator, hasCanvas} = context;
  return (event) => {
    const obj = JSON.parse(event.data);
    const {name: commandAnimator, type, cmd} = obj;
    if(animator == commandAnimator || cmd == 'finish' || cmd == 'clear'){
      switch(type){
        case 'draw':
          if(hasCanvas){
            buffer_command(obj);
          }
          break;
        case 'control':
          control(obj, context);
          break;
        case 'info':
          info(obj, context);
          break;
      }
    }
  };
}

function controls_button(context, action){
  const {animator} = context
  const button = document.createElement("input");
  button.type = "button";
  button.id = `${animator}_${action}`;
  button.value = action;
  const command = `animator ${action} ${animator}`;
  button.addEventListener("click", ({data}) => {context.socket.send(`${command}`)});
  return button;
}

function timing_label({animator}){
  const t = document.createElement("input");
  t.type = "text";
  t.id = `${animator}_timing`;
  t.value = '';
  t.size = 10;
  const l = document.createElement('label');
  l.textContent = 'timing';
  l.htmlFor = t.id;

  const br = document.createElement('br');
  const div = document.createElement('div');
  div.appendChild(l);
  div.appendChild(t);
  div.appendChild(br);
  return div;
}

function control(Command, context){
  const {cmd} = Command;
  switch(cmd){
    case "clear":
      console.log(`${context.animator} clear`);
      clear_controls(context);
      break;
    case "select":
      controls_select(Command, context);
      break;
    case "number":
    case "textbox":
    case "color":
    case "checkbox":
      console.log(`${context.animator} ...`);
      console.dir(Command);
      controls_input(Command, context);
      break;
    default:
      console.log(`Ignoring command ${cmd}`);
  }
}

function info(msg, {animator, element}){
  if("avg_frame_time" in msg){
    const input = element.querySelector(`#${animator}_timing`);
    input.value = msg.avg_frame_time;
  }
}

function clear_controls({controlDiv}){
  remove_all_children(controlDiv);
}

function remove_all_children(e){
  while(e.firstChild){
    e.removeChild(e.firstChild);
  }
}

function controls_select({id, name, label, options}, {controlDiv}){

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

  controlDiv.appendChild(l);
  controlDiv.appendChild(s);
  controlDiv.appendChild(br);
}

function controls_input(object, context){
  const {controlDiv, element} = context;
  const i = document.createElement("input");
  i.setAttribute("type", object.cmd);
  i.id = object.id;
  i.name = object.id;
  i.value = object.value;
  i.checked = object.is_checked;
  i.size = 10;
  const l = document.createElement('label');
  l.textContent = object.label;
  l.htmlFor = i.id;

  const command = `animator set ${object.name} ${object.field}`;
  let eventHandler;
  if(object.cmd == 'checkbox'){
    eventHandler = () => context.socket.send(`${command} ${i.checked}`);
  }else{
    eventHandler = () => context.socket.send(`${command} ${i.value}`);
  }
  i.addEventListener("change", eventHandler);

  const br = document.createElement('br');

  controlDiv.appendChild(l);
  controlDiv.appendChild(i);
  controlDiv.appendChild(br);
}
