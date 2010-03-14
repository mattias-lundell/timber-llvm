TIMBERC=timberc

EXAMPLES = Echo Echo2 Echo3 EchoServer EchoServer2 MasterMind Primes Reflex TCPClient PingTimeServers SantaProblem2

examples: 
	@for ex in $(EXAMPLES); do \
		$(TIMBERC) --make  $$ex; \
	done

local: 
	@for ex in $(EXAMPLES); do \
		../timberc --make  $$ex; \
	done
