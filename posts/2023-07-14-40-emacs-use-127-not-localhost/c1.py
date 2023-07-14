import socket
import select

host = 'localhost'
port = 11451

def start_server():
    server_start = socket.socket(socket.AF_INET6, socket.SOCK_STREAM)
    server_start.bind((host, port))
    server_start.listen(1)

    while True:
        try:
            readable, _, _ = select.select([server_start], [], [], 1.0)
            if server_start in readable:
                client, addr = server_start.accept()
                print('connect start {}'.format(addr))
                while True:
                    data = client.recv(1024)
                    if data == b'stop':
                        client.close()
                        print('connect close')
                        break
                    else:
                        client.sendall(b'hello')
        except KeyboardInterrupt:
            print('Ctrol-c')
            break

start_server()
