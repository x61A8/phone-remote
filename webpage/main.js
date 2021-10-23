// WebSockets
function getWS () {
    const url = new URL(window.location.href);
    const address = url.hostname;
    const wsPort = url.searchParams.get('ws');
    const wsUrl = 'ws://' + address + ':' + wsPort + '/';
    return new WebSocket(wsUrl);
}
const socket = getWS();

// Player Switching
let player = 1;
const playerButton = document.getElementById('player');
function switchPlayer () {
    if (player === 1) {
	player = 2;
    }
    else {
	player = 1;
    }
    playerButton.textContent = `Player ${player}`;
}
playerButton.addEventListener('click', e => {
    switchPlayer();
});

// Input Handling
function sendKey (key) {
    const message = key + player;
    socket.send(message);
}

const timeouts = new Map();
const mouseTracker = new Map();

function addButtonListeners (id, key) {
    const button = document.getElementById(id);
    button.addEventListener('pointerdown', e => {
	sendKey(key);
	timeouts.set(key, setTimeout(function repeat(){
	    if (mouseTracker.get(key))
	    {
		sendKey(key);
		setTimeout(repeat, 100);
	    }}, 100));
	mouseTracker.set(key, true);
    });
    button.addEventListener('pointerup', e => {
	clearTimeout(timeouts.get(key));
	mouseTracker.set(key, false);
    });
}

addButtonListeners('left', 'L');
addButtonListeners('up', 'U');
addButtonListeners('down', 'D');
addButtonListeners('right', 'R');
addButtonListeners('start', '*');
addButtonListeners('select', 'E');
addButtonListeners('b', 'B');
addButtonListeners('a', 'A');
