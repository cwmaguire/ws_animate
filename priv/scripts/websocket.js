// Create WebSocket connection.
const socket = new WebSocket("ws://localhost:8081/ws");

// Connection opened
socket.addEventListener("open", (event) => {
  socket.send("Hello Server!");
  socket.send("channel start");
  socket.send("channel sub draw");
});

// Listen for messages
socket.addEventListener("message", (event) => {
  console.log("Server: ", event.data);
});
