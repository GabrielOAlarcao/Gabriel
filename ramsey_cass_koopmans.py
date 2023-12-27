### Ramsey Cass-Koopmans
import matplotlib.pyplot as plt
plt.rcParams["figure.figsize"] = (11, 5)  #set default figure size
from numba import jit
import numpy as np

class Parameters:
    def __init__(self, alpha, beta, delta, gamma, T):
        self.alpha = alpha
        self.beta = beta
        self.delta = delta
        self.gamma = gamma
        self.T = T

class PolicySequence:
    def __init__(self, tau_c_seq, tau_k_seq, g_seq):
        self.tau_c_seq = tau_c_seq
        self.tau_k_seq = tau_k_seq
        self.g_seq = g_seq
        

class NeoclassicalGrowth:
  
  def __init__(self, params, policy_seq):
    self.params = params
    self.policy_seq = policy_seq
    
  @jit
  def solve_neoclassical_growth(self):
    alpha = self.params.alpha
    beta = self.params.beta
    delta = self.params.delta
    gamma = self.params.gamma
    T = self.params.T
    
    # steady state
    k_ss = (alpha / ((1 / beta) - (1 - delta)))**(1 / (1 - alpha))
    c_ss = k_ss**alpha - delta * k_ss

    # Algorithm parameters
    max_iter = 10000
    tol = 10**-3
    
    # Define Shooting Function: given initial condition (k0) and guess (c0), it simulates model using resource constraint and Euler equation
    def shoot(c0, k0):
        k = np.zeros(T)
        c = np.zeros(T)
        c[0] = c0  # guess
        k[0] = k0  # fixed

        for t in range(T - 1):
            k[t + 1] = k[t]**alpha + (1 - delta) * k[t] - c[t]
            if k[t + 1] < 0:
                k[t + 1] = 0.0001
            c[t + 1] = c[t] * ((1 + alpha * k[t + 1]**(alpha - 1) - delta) * beta)**(1 / gamma)

        return k, c

    # Upper and lower bound of guess C0 (make sure guesses are within bounds)
    c_lower = c_ss / 30  # lower bound
    k, c = shoot(c_lower, k0)
    if (k[-1] - k_ss) / k_ss < 0:
        print("lower bound too high")
    c_upper = c_ss  # upper bound
    k, c = shoot(c_upper, k0)
    if (k[-1] - k_ss) / k_ss > 0:
        print("upper bound too low")

    c_guess = (c_lower + c_upper) / 2  # initial guess

    for it in range(1, max_iter + 1):
        k, c = shoot(c_guess, k0)  # shoot

        dist = (k[-1] - k_ss) / k_ss  # distance in % from the SS
        if abs(dist) < tol:  # if dist smaller than tolerance we are done
            print("Iteration Finished:", it, "Distance:", dist)
            break

        # update guess
        if dist > 0:  # If capital is too high, must increase c0
            c_lower = c_guess
            c_guess = (c_guess + c_upper) / 2
        elif dist < 0:  # If capital is too low, must decrease c0
            c_upper = c_guess
            c_guess = (c_guess + c_lower) / 2
        # print("Iteration:", it, "Distance:", dist)
        
  @jit
  def solve_neoclassical_fiscal_pol(self):
    # Model parameters
    alpha = self.params.alpha
    beta = self.params.beta
    delta = self.params.delta
    gamma = self.params.gamma
    T = self.params.T

    tau_c_seq = self.policy_seq.tau_c_seq
    tau_k_seq = self.policy_seq.tau_k_seq
    g_seq = self.policy_seq.g_seq

    # Initial Steady State
    tau_k_0 = tau_k_seq[0]
    g_0 = g_seq[0]
    k_ss_0 = (alpha / ((1 / beta - 1) / (1 - tau_k_0) + delta))**(1 / (1 - alpha))
    c_ss_0 = k_ss_0**alpha - delta * k_ss_0 - g_0

    # Final Steady State
    tau_k_T = tau_k_seq[-1]
    g_T = g_seq[-1]
    k_ss_T = (alpha / ((1 / beta - 1) / (1 - tau_k_T) + delta))**(1 / (1 - alpha))
    c_ss_T = k_ss_T**alpha - delta * k_ss_T - g_T

    # Algorithm parameters
    max_iter = 10000
    tol = 10**-3

    # Define Shooting Function: given initial condition (k0) and guess (c0), it simulates model using resource constraint and Euler equation
    def shoot(c0, k0):
        k = np.zeros(T)
        c = np.zeros(T)
        c[0] = c0  # guess
        k[0] = k0  # fixed

        for t in range(T - 1):
            k[t + 1] = k[t]**alpha + (1 - delta) * k[t] - c[t] - g_seq[t]
            if k[t + 1] < 0:
                k[t + 1] = 0.0001
            c[t + 1] = c[t] * (((1 + tau_c_seq[t]) / (1 + tau_c_seq[t + 1])) *
                               (1 + (alpha * k[t + 1]**(alpha - 1) - delta) * (1 - tau_k_seq[t + 1])) * beta)**(1 / gamma)

        return k, c

    # Upper and lower bound of guess C0 (make sure guesses are within bounds)
    c_lower = c_ss_0 / 10  # lower bound
    k, c = shoot(c_lower, k_ss_0)
    if (k[-1] - k_ss_T) / k_ss_T < 0:
        print("lower bound too high")
    c_upper = c_ss_0 * 10  # upper bound
    k, c = shoot(c_upper, k_ss_0)
    if (k[-1] - k_ss_T) / k_ss_T > 0:
        print("upper bound too low")

    c_guess = (c_lower + c_upper) / 2  # initial guess

    for it in range(1, max_iter + 1):
        k, c = shoot(c_guess, k_ss_0)  # shoot

        dist = (k[-1] - k_ss_T) / k_ss_T  # distance in % from the SS
        if abs(dist) < tol:  # if dist smaller than tolerance we are done
            print("Iteration Finished:", it, "Distance:", dist)
            break

        # update guess
        if dist > 0:  # If capital is too high, must increase c0
            c_lower = c_guess
            c_guess = (c_guess + c_upper) / 2
        elif dist < 0:  # If capital is too low, must decrease c0
            c_upper = c_guess
            c_guess = (c_guess + c_lower) / 2
        # print("Iteration:", it, "Distance:", dist)  # Uncomment this line to display iteration information
