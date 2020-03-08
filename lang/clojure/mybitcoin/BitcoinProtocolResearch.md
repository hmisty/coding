# 比特币协议研究

Step by Step
1. 启动节点，发现其他节点，连接和握手


## Step 1. 比特币网络发现

### DNS种子

以下DNS种子服务来自于[bitcoin wiki](https://en.bitcoin.it/wiki/Satoshi_Client_Node_Discovery)：
```
seed.bitcoin.sipa.be
dnsseed.bluematt.me
dnsseed.bitcoin.dashjr.org
seed.bitcoinstats.com
seed.btcoin.jonasschnelli.ch
seed.btc.petertodd.org
```

每个种子服务可以通过dns解析出多个IP地址，比如：
```bash
$ nslookup seed.bitcoin.sipa.be
Server:		192.168.1.1
Address:	192.168.1.1#53

Non-authoritative answer:
Name:	seed.bitcoin.sipa.be
Address: 158.130.48.250
Name:	seed.bitcoin.sipa.be
Address: 72.135.224.85
Name:	seed.bitcoin.sipa.be
Address: 194.135.92.96
Name:	seed.bitcoin.sipa.be
Address: 51.89.103.66
Name:	seed.bitcoin.sipa.be
Address: 18.224.66.120
Name:	seed.bitcoin.sipa.be
Address: 157.245.164.235
...
Name:	seed.bitcoin.sipa.be
Address: 173.231.207.16
Name:	seed.bitcoin.sipa.be
Address: 95.217.9.184
```

随便选一个，看看通不通：
```
$ ping 95.217.9.184
PING 95.217.9.184 (95.217.9.184): 56 data bytes
64 bytes from 95.217.9.184: icmp_seq=0 ttl=46 time=250.410 ms
64 bytes from 95.217.9.184: icmp_seq=1 ttl=46 time=251.309 ms
64 bytes from 95.217.9.184: icmp_seq=2 ttl=46 time=454.843 ms
...
```

### 版本握手

比特币节点连接上之后第一件事就是要做这件事：[Version Handshake（版本握手）](https://en.bitcoin.it/wiki/Version_Handshake)。

比特币协议是二进制协议，而不是类似http的纯文本协议。所以，不好直接用 nc 来模拟交互。我们需要写一些clojure代码来帮助我们组装二进制字节流数据，并通过tcp socket和全节点进行互动。

因为要研究协议的最底层细节，所以我们尽量不使用第三方库封装好的方法，而是直接上手操作二进制数据和网络通信。

#### 第一步：发送version消息

版本握手的第一步是client向full node发送version消息。这个version消息的格式可以参考[协议文档](https://en.bitcoin.it/wiki/Protocol_documentation#version)。

> 比特币协议的消息都是由24 bytes的消息头（header）和不定长的消息体（payload）组成的。消息头的二进制格式如下：
> 
> | *长度(bytes)* |  *字段*  |  *类型*   |              *解释*                        |
> |---------------|----------|-----------|--------------------------------------------|
> |       4       | magic    | uint32\_t | 区分网络的魔法值，例如main-net是0xd9b4bef9 |
> |      12       | command  | char[12]  | 消息指令(以\0结尾的字符串)，例如"version"  |
> |       4       | length   | uint32\_t | payload的长度(以字节数bytes为单位)         |
> |       4       | checksum | uint32\_t | 校验值=sha256(sha256(payload))的头4个字节  |

要组装这样的二进制数据，我们选择byte list作为基本的数据结构，它可以很方便地生成、转换和操作：
```clojure
mybitcoin.core=> (def magic '(0xf9 0xbe 0xb4 0xd9))
#'mybitcoin.core/magic

mybitcoin.core=> magic
(249 190 180 217)
```

对于数值，需要注意所谓endian（字节序）的问题。little endian（小字节序）的低位在前，而big endian（大字节序，又称网络字节序network byte order）则是高位在前。

比如，比特币主网的magic数值是0xd9b4bef9，而消息头里的magic字段采用little endian，所以4字节list是'(0xf9 0xbe 0xb4 0xd9)。

又比如version消息的payload有个协议版本号，比如是60002（10进制），转换成十六进制就是0xea62，那么，转换成4字节list就是'(0x62 0xea 0x00 0x00)。如果用big endian呢，reverse一下就好了，(reverse '(0x62 0xea 0x00 0x00)) => '(0x00 0x00 0xea 0x62)。

而对于字符串而言，转换为byte list就是直接对应：第一个字符对应第一个字节，第二个字符对应第二个字节，依次类推。

byte list和string以及byte-array（网络操作底层数据结构）的转换很方便：
```clojure
mybitcoin.core=> (map byte "version")
(118 101 114 115 105 111 110)

mybitcoin.core=> (byte-array (map byte "version"))
#object["[B" 0x575df069 "[B@575df069"]

mybitcoin.core=> (.getBytes "version")
#object["[B" 0x671e33 "[B@671e33"]

mybitcoin.core=> (list* (.getBytes "version"))
(118 101 114 115 105 111 110)
```

而其他一些字段，我们需要写一些helper function来帮助我们来简单快速的构建所需的byte list，比如：
```clojure
(defn byte4<-int32_t
  "Converts an int32_t number [n] to 4 bytes."
  [n]
  (map (fn [bits] (bit-and 0xff (bit-shift-right n bits))) [0 8 16 24]))
```
等等。

而checksum的计算要用到双sha256哈希，我们用java自带的security库来完成：
```clojure
(defn dhash
  "Returns SHA256(SHA256(bytes)) of the byte-array [bytes]."
  [bytes]
  (let [m (java.security.MessageDigest/getInstance "SHA-256")]
    (.digest m (.digest m bytes))))

(defn checksum
  "Returns the checksum for the [payload] bytes."
  [payload]
  (take 4 (dhash (byte-array payload))))
```

> version消息的payload字段比较多，具体可以参考上面给出的协议文档。
>
> | *长度(bytes)* |  *字段*  |  *类型*   |              *解释*                        |
> |---------------|----------|-----------|--------------------------------------------|
> |       4       | version  | int32\_t  | 协议版本号，比如60002                      |
> |       8       | services | uint64\_t |                                            |
> |       8       | timestamp| int64\_t  |                                            |
> |      26       | addr\_recv| net\_addr  |                                            |

协议版本号 >= 106 还会包含下面的字段：
> | *长度(bytes)* |  *字段*  |  *类型*   |              *解释*                        |
> |---------------|----------|-----------|--------------------------------------------|
> |      26       | addr\_from| net\_addr  |                                            |
> |       8       | nonce    | uint64\_t |                                            |
> |       ?       | user\_agent| var\_str | User Agent字符串(=0x00 如果字符串为"")    |
> |       4       | start\_height| int32\_t |                                            |

比如我们可以这样构建一个payload：
```clojure
;; message payload of "version"
(def version-payload
  (let [protocol-version (byte4<-int32_t 60002)
        services (byte8<-int64_t 1)
        timestamp (byte8<-current_timestamp)
        receiver-address (concat services
                                 (byte18<-ip_port "0.0.0.0" 8333))
        sender-address (concat services
                                 (byte18<-ip_port "0.0.0.0" 8333))
        node-id (byte8<-nonce)
        sub-version (bytes<-var_str "/Satoshi:0.7.2/")
        block-height (byte4<-int32_t 212672)]
    (concat protocol-version services timestamp receiver-address sender-address node-id sub-version block-height)))
```

然后，我们根据这个payload构建header：
```clojure
;; message header
(def version-header
  (let [magic (byte4<-int32_t 0xd9b4bef9)
        command (bytes<-n_str 12 "version")
        payload-length (byte4<-int32_t (count version-payload))
        payload-checksum (checksum version-payload)]
    (concat magic command payload-length payload-checksum)))
```

简单拼起来就是完整的version消息了：
```clojure
;; construct the version message bytes
(def version-message
  (concat version-header version-payload))
```

有了这些，我们就可以通过向上面选择的一个full node的IP地址发送version消息，来观察是否能够收到什么回应：
```clojure
;; client node over tcp/ip
(defn start-client "Starts a client node." []
  (let [host "95.217.9.184"
        port 8333
        header (byte-array 24)]
    (with-open [socket (Socket. host port)
                out (io/output-stream socket)
                in (io/input-stream socket)]
      (.write out (byte-array version-message))
      (.flush out)
      (.read in header)
      (print-hex header))))
```

上面，我们通过require引用了clojure.lang.io，并import了Java.net的Socket对象。我们还写了print-hex来用十六进制打印二进制的bytes以便于查看：
```clojure
(defn print-hex
  "Displays the byte-list in lowercase HEX string format."
  [byte-list]
  (apply str (map (fn [byte] (format "%02x" byte)) byte-list)))
```

让我们在REPL里运行一下(start-client)，我们会收到类似如下的header：
```
f9beb4d976657273696f6e0000000000660000009e53c190
```

对照协议拆解一下：
```
f9 be b4 d9                             - 比特币main-net的魔法数字 0xd9b4bef9
76 65 72 73 69 6f 6e 00 00 00 00 00     - 对方回复的还是"version"消息
66 00 00 00                             - 0x66 = 102字节，这是接下来payload的长度
9e 53 c1 90                             - payload的checksum
```

然后我们简单修改一下代码，接收一下payload数据看看：
```clojure
;; client node over tcp/ip
(defn start-client "Starts a client node." []
  (let [host "95.217.9.184"
        port 8333
        header (byte-array 24)
        payload (byte-array 0x66)]
    (with-open [socket (Socket. host port)
                out (io/output-stream socket)
                in (io/input-stream socket)]
      (.write out (byte-array version-message))
      (.flush out)
      (.read in header)
      (.read in payload)
      (print-hex (concat header payload)))))
```

运行一下，收到：
```
f9beb4d976657273696f6e00000000006600000087e724c07f1101000d04000000000000942c655e00000000010000000000000000000000000000000000ffff00000000208d0d04000000000000000000000000000000000000000000000000073b5d91c5c8d7d4102f5361746f7368693a302e31382e312f1f79090001
```

删掉前面24字节的header，只留下后面102字节的payload数据，拆解如下：
```
7f 11 01 00                                            - 0x01117f = 70015版本的协议
0d 04 00 00 00 00 00 00                                - 0x040d = 1037五种服务全都有
94 2c 65 5e 00 00 00 00                                - 时间戳
01 00 00 00 00 00 00 00                                - 我方服务
   00 00 00 00 00 00 00 00 00 00 ff ff                 - IPv6前缀
   00 00 00 00                                         - IPv4
   20 8d                                               - 端口
0d 04 00 00 00 00 00 00                                - 对方服务
   00 00 00 00 00 00 00 00 00 00 00 00                 - IPv6前缀，直接留空
	 00 00 00 00                                         - IPv4，直接留空
	 00 00                                               - 端口，直接留空
07 3b 5d 91 c5 c8 d7 d4                                - nonce随机值
10 2f 53 61 74 6f 73 68 69 3a 30 2e 31 38 2e 31 2f     - 字符串"/Satoshi:0.18.1/"
1f 79 09 00                                            - 0x09791f = 620831 区块高度
01                                                     - 01 = true, relay宣告(BIP 0037)
```


