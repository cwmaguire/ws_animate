"use strict";
// Create WebSocket connection.
const socket = new WebSocket("ws://localhost:8081/ws");
var c;
var channel;
var animatorIndex = 1;
var receive_buffer = [];
var draw_buffer = [];

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
  const qsParams = {channel: channel, animator: name};
  button.addEventListener("click", (event) => { openNewWindow("animation_controls", qsParams) })
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

function square({ctx}, {x, y, w, h, style}){
  //console.log(`square: x: ${x}, y: ${y}, w: ${w}, h: ${h}, style: ${style}`);
  ctx.strokeSyle = style;
  ctx.strokeRect(x, y, w, h);
}

function square_filled({ctx}, {x, y, w, h, style}){
  ctx.fillStyle = style;
  ctx.fillRect(x, y, w, h);
}

function square_gradient({ctx}, Command){
  var {x, y, w, h, style : {gx1, gy1, gx2, gy2, stop1: {stop: s1, color: c1}, stop2: {stop: s2, color: c2}}} = Command;
  var gradient = ctx.createLinearGradient(gx1, gy1, gx2, gy2);
  gradient.addColorStop(s1, c1);
  gradient.addColorStop(s2, c2);
  ctx.fillStyle = gradient;
  ctx.fillRect(x, y, w, h);
}

function circle({ctx}, {x, y, r, style}){
  //console.log(`square: x: ${x}, y: ${y}, w: ${w}, h: ${h}, style: ${style}`);
  ctx.strokeSyle = style;
  ctx.beginPath();
  ctx.ellipse(x, y, r, r, 0, 0, 2 * Math.PI);
  ctx.stroke();
}

function line({ctx}, {x1, y1, x2, y2}){
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
  openNewWindow("animation_controls");
}

function openNewWindow(name, qsParams) {
  const top = window.screenTop;
  const left = window.screenLeft;
  const url = `http://localhost:8081/html/${name}.html`;
  const qs = new URLSearchParams(qsParams).toString();
  console.log(`Query string for new window: ${qs}`);
  const newWindow = window.open(`${url}?${qs}`, "_blank", `top=${top - 50},left=${left + 50},width=600,height=400,status=1,toolbar=1`);
  newWindow.addEventListener("load", (event) => {
    socket.addEventListener("close", (event) => { newWindow.close(); });
  });
}
