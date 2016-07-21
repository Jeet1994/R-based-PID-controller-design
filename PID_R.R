pv = function(pv.prev, u, tt) {
  out = pv.prev*1.1 + .5           # exponential growth + linear growth 
  out = out - 0.1*u                # the control response
  out = out + .5*runif(length(tt)) # a little noise, just for fun
  
  if (out < 0) out = 0             # keep values positive
  return(out)
}

Kp = 10                 # proportional gain
Ti = 1                  # integral time
Td = 0.01               # derivative time

# simulation parameters
dt = .1                 # time step
tt = seq(0, 100, by=dt) # time vector

# initialize the following to a vector of zeros
# as long as the time variable tt
# - PV, process variable
# - U, control output
# - E, error
# - EI, error integral
# - ED, error derivative
PV = U = E = EI = ED = rep(0, length(tt))
PV[1] = 5 # initial state of the process variable

SP = rep(10, length(tt))
SP[which(tt >= 30)] = 5
SP[which(tt >= 60)] = 20

for (k in 2:length(tt)) {
  PV[k] = pv(PV[k-1], U[k-1], tt[k])
  E[k] = PV[k] - SP[k]
  
  EI[k] = EI[k-1] + E[k]*dt  # integral
  ED[k] = (E[k] - E[k-1])/dt # derivative
  
  U[k] = Kp*(E[k] + (1/Ti)*sum(E*dt) + Td*ED[k]) 
  
  if (U[k] < 0) U[k] = 0
}

