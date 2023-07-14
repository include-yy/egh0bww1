const net = require('net')

const server = net.createServer()

server.on('connection', (socket) => {
    console.log('con start')

    socket.on('data', (data) => {
	console.log('get message:', data.toString())
	socket.write('hello\n')
    })

    socket.on('close', () => {
	console.log('con closed')
    })
})

server.listen(11451, () => {
    console.log('start server ...')
})
