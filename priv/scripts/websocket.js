"use strict";
// Create WebSocket connection.
const socket = new WebSocket("ws://localhost:8081/ws");
var c;
var channel;
var animatorIndex = 1;
var receive_buffer = [];
var draw_buffer = [];
var click_targets = [];
var canvas = document.querySelector('#canvas1');

canvas.addEventListener("click", (e) => get_click_target(e));

// Connection opened
socket.addEventListener("open", (event) => {
  socket.send("channel start");
  socket.send("channel sub draw");
  socket.send("animator list");
  //socket.send("channel sub log");
  //socket.send("animator add animator1 a");
  requestAnimationFrame(render_buffer);
});

socket.addEventListener("message", (event) => {
  let obj = JSON.parse(event.data);
  switch(obj.type){
    case "draw":
      if(obj.cmd == "finish"){
        draw_buffer = receive_buffer;
        receive_buffer = [];
      }else{
        receive_buffer.push(obj);
      }
      break;
    case "info":
      info(obj);
      break;
    case "log":
      console.log(obj.log);
      break;
    }
});

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

socket.addEventListener("error", (event) => {
  console.log("Websocket error: ", event);
});

socket.addEventListener("close", (event) => {
  console.log("Websocket close. Code: ", event.code, ". Reason: \"", event.reason, "\". Clean? ", event.wasClean);
});

function notNullOrUndefined(val){
  return val != null; // checks for null and undefined since they both equate to null
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

function render_buffer(_timestamp){
  const buffer = draw_buffer;
  draw_buffer = [];
  buffer.forEach(draw);
  requestAnimationFrame(render_buffer);
}

function draw(Command){
  const {cmd} = Command;
  //console.log(cmd);
  c = ctx();
  switch(cmd){
    case "clear":
      clear(c);
      click_targets = [];
      break;
    case "square":
      square(c, Command);
      break;
    case "square_filled":
      square_filled(c, Command);
      break;
    case "square_gradient":
      square_gradient(c, Command);
      break;
    case "circle":
      circle(c, Command);
      break;
    case "line":
      line(c, Command);
      break;
    case "text":
      text(c, Command);
      break;
    default:
      console.log(`Ignoring command ${cmd}`);
  }
}

function clear({ctx, w, h}){
  ctx.clearRect(0, 0, w, h);
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

function ctx(){
  const canvas = document.getElementById("canvas1");
  const ctx = canvas.getContext("2d");
  return {ctx: ctx, w: canvas.width, h: canvas.height};
}

function animationControls(){
  open_new_window("animation_controls");
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
  const {offsetX: x, offsetY: y} = event;
  console.log(`Click on ${x}, ${y}`);
  const is_click_target_ = (ct) => is_click_target(ct, {x, y});
  const maybe_click_target = click_targets.filter(is_click_target_)[0];
  if(maybe_click_target){
    console.log(`found ${maybe_click_target.name}`);
    document.querySelector(`#animator_controls_button_${maybe_click_target.name}`).click();
  }else{
    console.log('-------------------------------');
    console.dir(click_targets);
    console.dir(event);
    console.log('-------------------------------');
  }
}

function is_click_target(shape, {x, y}){
  switch(shape.type){
    case 'square':
      console.dir(shape);
      console.dir(`Is shape at ${x}, ${y}?`);
      return square_hittest({x, y}, shape);
      break;
    case 'circle':
      return circle_hittest({x, y}, shape);
      break;
    default:
      console.dir(shape);
      console.dir(`Is shape at ${x}, ${y}?`);
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
