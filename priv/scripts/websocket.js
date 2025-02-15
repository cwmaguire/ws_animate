"use strict";
var context2dWithDims;
var channel;
var canvas;
var loadSaveDiv;
var animatorControlsDiv;
var animatorControlsButtonDiv;
var animatorIndex = 1;
var receive_buffer = [];
var draw_buffer = [];
var click_targets = [];

create_canvas_and_controls();

const socket = new WebSocket("ws://localhost:8081/ws");
socket.addEventListener("open", websocket_open);
socket.addEventListener("message", websocket_message);
socket.addEventListener("error", websocket_error);
socket.addEventListener("close", websocket_close);

function websocket_open(event){
  socket.send("channel start");
  socket.send("channel sub draw");
  socket.send("animator list");
  requestAnimationFrame(animation_frame);
}

function websocket_message(event){
  let obj = JSON.parse(event.data);
  switch(obj.type){
    case "draw":
      buffer_command(obj);
      break;
    case "info":
      info(obj);
      break;
    case "log":
      console.log(obj.log);
      break;
  }
}

function websocket_error(event){
  console.log("Websocket error: ", event);
}

function websocket_close(event){
  console.log("Websocket close. Code: ", event.code, ". Reason: \"", event.reason, "\". Clean? ", event.wasClean);
}

function info(obj){
  if("channel_name" in obj){
    console.log(`channel name info: ${event.data}`);
    channel = obj.channel_name;
  }else if("animators" in obj){
    console.log(`animators info: ${event.data}`)
    animator_buttons(obj.animators);
  }else if("animator_name" in obj){
    console.log(`animator_name info: ${event.data}`)
    animator_server_button(obj.animator_name);
  }
}

function animator_buttons(animators){
  document.getElementById("animator_button_div").childNodes.forEach(({child}) => child.remove());
  animators.forEach((animator) => {animator_button(animator)});
}

function animator_button({name, short_name}){
  const button = document.createElement("input");
  button.id = `animator_button_${name}`;
  button.type = "button";
  button.value = name;
  button.addEventListener("click",
    (event) => {
      socket.send(`animator add ${name} ${short_name}_${animatorIndex.toString()}`);
      animatorIndex += 1;
    });
  document.getElementById("animator_button_div").appendChild(button);
}

function animator_server_button(name){
  const button = document.createElement("input");
  button.id = `animator_controls_button_${name}`;
  button.type = "button";
  button.value = name;
  const qsParams = {channel, animator: name};
  button.addEventListener("click",
    (event) => {
      if('old_window' in button){
        button.old_window.focus();
      }else{
        open_new_window("animation_controls", qsParams, button);
      }
    })
  document.getElementById("animator_controls_button_div").appendChild(button);
}

function buffer_command(obj){
  if(obj.cmd == "finish"){
    draw_buffer = receive_buffer;
    receive_buffer = [];
  }else{
    receive_buffer.push(obj);
  }
}

function animation_frame(_timestamp){
  render_buffer();
  requestAnimationFrame(animation_frame);
}

function render_buffer(_timestamp){
  const buffer = draw_buffer;
  draw_buffer = [];
  buffer.forEach(draw);
}

function draw(Command){
  const {cmd} = Command;
  switch(cmd){
    case "clear":
      clear(context2dWithDims);
      break;
    case "square":
      square(context2dWithDims, Command);
      break;
    case "square_filled":
      square_filled(context2dWithDims, Command);
      break;
    case "square_gradient":
      square_gradient(context2dWithDims, Command);
      break;
    case "circle":
      circle(context2dWithDims, Command);
      break;
    case "line":
      line(context2dWithDims, Command);
      break;
    case "text":
      text(context2dWithDims, Command);
      break;
    default:
      console.log(`Ignoring command ${cmd}`);
  }
}

function clear({ctx, w, h}){
  ctx.clearRect(0, 0, w, h);
  click_targets = [];
}

function square({ctx}, command){
  const {x, y, w, h, style, name} = command;
  ctx.strokeSyle = style;
  ctx.strokeRect(x, y, w, h);
  add_click_target({...command, type: 'square'});
}

function square_filled({ctx}, command){
  const {x, y, w, h, style, name} = command;
  ctx.fillStyle = style;
  ctx.fillRect(x, y, w, h);
  add_click_target({...command, type: 'square'});
}

function square_gradient({ctx}, command){
  var {x, y, w, h, style : {gx1, gy1, gx2, gy2, stop1: {stop: s1, color: c1}, stop2: {stop: s2, color: c2}}} = command;
  var gradient = ctx.createLinearGradient(gx1, gy1, gx2, gy2);
  gradient.addColorStop(s1, c1);
  gradient.addColorStop(s2, c2);
  ctx.fillStyle = gradient;
  ctx.fillRect(x, y, w, h);
  add_click_target({...command, type: 'square'});
}

function circle({ctx}, command){
  const {x, y, r, style, name} = command;
  //console.log(`square: x: ${x}, y: ${y}, w: ${w}, h: ${h}, style: ${style}`);
  ctx.strokeSyle = style;
  ctx.beginPath();
  ctx.ellipse(x, y, r, r, 0, 0, 2 * Math.PI);
  ctx.stroke();
  add_click_target({...command, type: 'circle'});
}

function line({ctx}, command){
  const {x1, y1, x2, y2} = command;
  ctx.strokStyle = 'black';
  ctx.beginPath();
  ctx.moveTo(x1, y1);
  ctx.lineTo(x2, y2);
  ctx.stroke();
}

function text({ctx}, {text, x, y, font_size, font_color}){
  ctx.font = `${font_size}, 'Courier New', monospace;`;
  ctx.fillStyle = font_color;
  ctx.fillText(text, x, y);
}

function open_new_window(name, qsParams, button) {
  const top = window.screenTop;
  const left = window.screenLeft;
  const url = `http://localhost:8081/html/${name}.html`;
  const qs = new URLSearchParams(qsParams).toString();
  console.log(`Query string for new window: ${qs}`);
  const newWindow = window.open(`${url}?${qs}`, "_blank", `top=${top - 50},left=${left + 50},width=600,height=400,status=1,toolbar=1`);
  newWindow.addEventListener("load", (event) => {
    socket.addEventListener("close", (event) => { newWindow.close(); });
  });
  button.old_window = newWindow;
}

function add_click_target(shape){
  click_targets.unshift(shape);
}

function get_click_target(event){
  const temp_click_targets = click_targets.slice();
  const {offsetX: x, offsetY: y} = event;
  const is_click_target_ = (ct) => is_click_target(ct, {x, y});
  const maybe_click_target = temp_click_targets.filter(is_click_target_)[0];
  if(maybe_click_target){
    document.querySelector(`#animator_controls_button_${maybe_click_target.name}`).click();
  }
}

function is_click_target(shape, {x, y}){
  switch(shape.type){
    case 'square':
      return square_hittest({x, y}, shape);
      break;
    case 'circle':
      return circle_hittest({x, y}, shape);
      break;
  }
}

function square_hittest({x, y}, square){
  return x >= square.x &&
         x <= square.x + square.w &&
         y >= square.y &&
         y <= square.y + square.h;
}

function circle_hittest({x: x1, y: y1}, {x: x2, y: y2, r}){
  return r >= Math.hypot(x1 - x2, y1 - y2);
}

function create_canvas_and_controls(){

  loadSaveDiv = div('load_save_div');
  animatorControlsDiv = div('animator_controls_div');
  animatorControlsButtonDiv = div('animator_controls_button_div');

  canvas = document.createElement('canvas');
  canvas.id = 'canvas1';
  canvas.width = '800px';
  canvas.height = '700px';

  const ctx = canvas.getContext("2d");
  context2dWithDims = {ctx: ctx,
                       w: canvas.width,
                       h: canvas.height};
  canvas.addEventListener("click", get_click_target);
}

function div(id){
  const div = document.createElement('div');
  div.id = 'load_save_div';
  document.body.appendChild(div);
  return div;
}
