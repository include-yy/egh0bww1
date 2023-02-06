import time
import threading

IDLE = 0
RX_START = 1
RECEIVE = 2
RX_STOP = 3
TX_START = 4
SEND = 5
TX_STOP = 6

def sleep250():
    time.sleep(0.25)
def sleep750():
    time.sleep(0.75)

def sleep1000():
    time.sleep(1.0)

def byte2int(array):
    acc = 0
    for i in range(0, 8):
        acc = acc * 2 + array[7 - i]
    return acc

def int2byte(val):
    arr = [0, 0, 0, 0, 0, 0, 0, 0]
    for i in range(0, 8):
        arr[i] = val & 1
        val = val // 2
    return arr

class UART:

    def __init__(self, TX, RX):
        # 绑定数据线

        self.TX = TX
        self.RX = RX

    def send(self, tx_byte):
        state = IDLE
        # 时钟与状态机
        while state != TX_STOP:
            if (state == IDLE):
                state = TX_START
            elif (state == TX_START):
                self.TX[0] = 0
                sleep1000()
                state = SEND
            elif (state == SEND):
                self.TX[0] = tx_byte[0]
                sleep1000()
                self.TX[0] = tx_byte[1]
                sleep1000()
                self.TX[0] = tx_byte[2]
                sleep1000()
                self.TX[0] = tx_byte[3]
                sleep1000()
                self.TX[0] = tx_byte[4]
                sleep1000()
                self.TX[0] = tx_byte[5]
                sleep1000()
                self.TX[0] = tx_byte[6]
                sleep1000()
                self.TX[0] = tx_byte[7]
                sleep1000()
                state = TX_STOP
            else:
                raise Exception('wtfsend')
        self.TX[0] = 1
        sleep1000()
        state = IDLE
        return

    def rec(self):
        state = IDLE
        RX_cache = self.RX[0]
        data = [0, 0, 0, 0, 0, 0, 0, 0]
        while state != RX_STOP:
            if (state == IDLE):
                if (self.RX[0] == 0 and RX_cache == 1):
                    state = RX_START
                else:
                    RX_cache = self.RX[0]
                    sleep250()
            elif (state == RX_START):
                state = RECEIVE
                sleep1000()
            elif (state == RECEIVE):
                sleep250()
                data[0] = self.RX[0]
                sleep1000()
                data[1] = self.RX[0]
                sleep1000()
                data[2] = self.RX[0]
                sleep1000()
                data[3] = self.RX[0]
                sleep1000()
                data[4] = self.RX[0]
                sleep1000()
                data[5] = self.RX[0]
                sleep1000()
                data[6] = self.RX[0]
                sleep1000()
                data[7] = self.RX[0]
                sleep750()
                state = RX_STOP
            else:
                raise Exception('wtfrecieve')
        sleep1000()
        state = IDLE
        return data

rx = [1]
tx = [1]

u1 = UART(tx, rx)
u2 = UART(rx, tx)

def computer1():
    cnt = 0
    while cnt < 3:
        cnt = cnt + 1
        cntb = int2byte(cnt)
        print ("computer1: start send %d" % cnt)
        u1.send(cntb)
        print ("computer1: end send")
        print ("computer1: start receive")
        cntb = u1.rec()
        cnt = byte2int(cntb)
        print ("computer1: end receive %d" % cnt)

def computer2():
    cnt = 0
    while cnt < 3:
        print ("computer2: start receive")
        cntb = u2.rec()
        cnt = byte2int(cntb)
        print ("computer2: end receive %d" % cnt)
        cnt = cnt + 1
        cntb = int2byte(cnt)
        print ("computer2: start send %d" % cnt)
        u2.send(cntb)
        print ("computer2: end send")

x = threading.Thread(target=computer2)
x.start()
time.sleep(3)

x = threading.Thread(target=computer1)
x.start()
