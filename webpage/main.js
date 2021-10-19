const url = new URL(window.location.href);
const address = url.hostname;
const wsPort = url.searchParams.get("ws");
