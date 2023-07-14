import socket
import select

def handle_client(client):
    while True:
        data = client.recv(1024)
        if data == b'stop':
            client.close()
            print('connect close')
            break
        else:
            client.sendall(b'hello')

def start_server():
    server = socket.create_server(('', 11451), family=socket.AF_INET6,
                                  dualstack_ipv6=True)
    print('server start ...')

    while True:
        client, addr = server.accept()
        print('conn start')
        handle_client(client)
        print('conn close')

start_server()
