// Create WebSocket connection.
const socket = new WebSocket("ws://localhost:8081/ws");

// Connection opened
socket.addEventListener("open", (event) => {
  socket.send("Hello Server!");
  socket.send("channel start");
  socket.send("channel sub draw");
  socket.send("animator add animator1 a");
});

// Listen for messages
socket.addEventListener("message", (event) => {
  //console.log(`Server: `, event.data);
  obj = JSON.parse(event.data);
  //console.log(`JSON: ${obj.cmd}`);
  if(obj.type == "draw"){
    //console.log("Type is draw");
    draw(obj);
  }else{
    //console.log(`Type is: ${obj.type}`);
  }
});

function notNullOrUndefined(val){
  return val != null; // checks for null and undefined since they both equate to null
}

function draw(Command){
  const {cmd} = Command;
  //console.log(`Command.cmd ${Command.cmd}`);
  //console.log(`cmd is '${cmd}', ${typeof cmd}`);
  c = ctx();
  switch(cmd){
    case "clear":
      clear(c);
      break;
    case "square":
      square(c, Command);
      break;
    default:
      console.log(`Ignoring command ${cmd}`);
  }
}

function clear({ctx, w, h}){
  ctx.clearRect(0, 0, w, h);
}

function square({ctx}, {x, y, w, h, style}){
  //console.log(`Style is ${style}`);
  console.log("square");
  ctx.strokeSyle = style;
  ctx.strokeRect(x, y, w, h);
}

function ctx(){
  const canvas = document.getElementById("canvas1");
  const ctx = canvas.getContext("2d");
  return {ctx: ctx, w: canvas.width, h: canvas.height};
}
