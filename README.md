# phone-remote
A program to allow a smartphone to be used as a PC input device.

## How it works
When the program is launched a web server is created. 
A URL is generated for phones(or other machines) on the local network to connect to the server. 
When the webpage is opened a WebSocket connection is established between the phone and the host.

The user tapping on the buttons will send messages over the WebSocket connection to the server.
The server will translate the received message into the corresponding SendInput (Windows API) call.
Windows will send a KeyEvent to the focused window.
This is similar to how a keypress on a keyboard will be received by the focused window.

## The Common Lisp Omnificient GUI (CLOG)
The program has three main parts:
1. The Web GUI (HTML, CSS, JavaScript)
2. The server with program logic (Common Lisp)
3. A message protocol to communicate between the two. These messages are exchanged over WebSockets.

CLOG improves upon this in two major ways.
Firstly, a GUI DSL is provided, which allows you to avoid writing HTML or CSS.
Secondly, the messaging protocol is automatically created and managed.
This means you do not have to manually synchronize the JS code and the CL code when the protocol changes.

## Libraries used
All of these except win32 are available through quicklisp or your CL implementation.
* [win32](https://github.com/Zulu-Inuoe/win32) - Common Lisp bindings of the Windows API
* [CFFI](https://cffi.common-lisp.dev/manual/html_node/) - C Foreign Function Interface - To use the win32 bindings
* [Hunchentoot](https://edicl.github.io/hunchentoot/) - Popular Common Lisp web server
* [Hunchensocket](https://github.com/joaotavora/hunchensocket) - An addon to Hunchentoot that adds WebSockets support
* [cl-qrencode](https://github.com/jnjcc/cl-qrencode) - A QR encoder for Common Lisp. Used to conveniently send URLs from the host machine to a phone

## Related reading
* [WebSockets API](https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API) - The core of this program
* [CLOG](https://github.com/rabbibotton/clog) - An advanced version of the same idea
* [Windows SendInput function](https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-sendinput) - Allows emulation of keyboard events through software
* [PC Scan Code Reference](https://web.archive.org/web/20200119164207/http://philipstorr.id.au/pcbook/book3/scancode.htm) - The table below, not the diagram
