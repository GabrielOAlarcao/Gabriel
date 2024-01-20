## Simulation Model
# Economics with Heterogeneous Interacting Agents
import numpy as np
import matplotlib.pyplot as plt

class FirmSimulation:
    def __init__(self, Ni, Time, gamma, phi, Pbar, r):
        self.Ni = Ni
        self.Time = Time
        self.gamma = gamma
        self.phi = phi
        self.Pbar = Pbar
        self.r = r
        self.A = np.ones((Ni, 1))
        self.K = np.ones((Ni, 1))
        self.B = np.zeros((Ni, 1))
        self.I = np.zeros((Ni, 1))
        self.P = np.zeros((Ni, 1))
        self.Y = np.zeros((Ni, 1))
        self.Z = 2 * np.random.rand(Ni, 1) + Pbar
        self.YY = np.zeros((Time, 1))
        self.AA = np.zeros((Time, 1))
        self.BB = np.zeros((Time, 1))
        
    def run_simulation(self):
        for t in range(1, self.Time):
            self.I = self.gamma * self.Z
            self.K = self.K + self.I
            self.Y = self.phi * self.K
            self.B = self.K - self.A
            self.B[self.B < 0] = 0
            self.P = 2 * np.random.rand(self.Ni, 1) + self.Pbar
            self.Z = self.P * self.Y - self.r * self.K
            self.A = self.A + self.Z
            self.Z[self.A < 0] = 0
            self.K[self.A < 0] = 1
            self.A[self.A < 0] = 1
            self.YY[t] = np.sum(self.Y)
            self.AA[t] = np.sum(self.A)
            self.BB[t] = np.sum(self.B)
            
    def plot_results(self, variable_name):
        time_steps = np.arange(1, self.Time)
        variable_values = getattr(self, variable_name)[1:]
        
        plt.figure(figsize=(10, 4))
        
        plt.plot(time_steps, variable_values, label=variable_name)
        plt.title(f'{variable_name} Over Time')
        plt.xlabel('Time')
        plt.ylabel(variable_name)
        plt.legend()
        
        plt.show()

# Example usage:
Ni = 100  # Number of firms
Time = 1000  # Number of simulations
gamma = 1.1  # Investment accelerator
phi = 0.1  # Capital productivity
Pbar = 0.01  # Price constant
r = 0.1  # Interest rate

simulation = FirmSimulation(Ni, Time, gamma, phi, Pbar, r)
simulation.run_simulation()
simulation.plot_results('YY')  # Aggregate Production
simulation.plot_results('AA')   # Net Worth
simulation.plot_results('BB')   # Capital


## -------------------- Monte Carlo Simulated Model --------------------------- ##
class MonteCarloFirmSimulation:
    def __init__(self, Time, Ni, MC, gamma, phi, Pbar, delta, rbar, seed=None):
        self.Time = Time
        self.Ni = Ni
        self.MC = MC
        self.gamma = gamma
        self.phi = phi
        self.Pbar = Pbar
        self.delta = delta
        self.rbar = rbar

        # Set random seed for reproducibility
        np.random.seed(seed)

        # Allocate aggregate variables / initial conditions
        self.YY = np.zeros((Time, MC))  # aggregate production
        self.AA = np.zeros((Time, MC))  # aggregate net worth
        self.BB = np.zeros((Time, MC))  # aggregate debt
        self.LEV = np.zeros((Time, MC))  # leverage

    def run_simulation(self):
        for mc in range(self.MC):
            # Allocate individual variables / initial conditions
            A = np.ones((self.Ni, 1))  # net worth
            K = np.ones((self.Ni, 1))  # capital
            B = np.zeros((self.Ni, 1))  # debt
            I = np.zeros((self.Ni, 1))  # investment
            P = np.zeros((self.Ni, 1))  # price
            Y = np.zeros((self.Ni, 1))  # production
            Z = 2 * np.random.rand(self.Ni, 1) + self.Pbar  # profit

            # Main program
            for t in range(1, self.Time):
                I = self.gamma * Z  # investment choice
                I[I < 0] = 0
                K = (1 - self.delta) * K + I  # capital accumulation
                Y = self.phi * K  # production
                B = K - A  # debt
                B[B < 0] = 0  # self-financed firms
                P = 2 * np.random.rand(self.Ni, 1) + self.Pbar  # stochastic price
                r = self.rbar + self.rbar * (B / np.maximum(A, 1e-6)) ** self.rbar
                Z = P * Y - r * K  # profit
                A = A + Z  # net worth
                Z[A < 0] = 0  # entry condition
                K[A < 0] = 1  # entry condition
                A[A < 0] = 1  # entry-exit process
                self.YY[t, mc] = np.sum(Y)  # aggregate production
                self.AA[t, mc] = np.sum(A)  # aggregate net worth
                self.BB[t, mc] = np.sum(B)  # aggregate debt
                self.LEV[t, mc] = np.sum(self.BB[t, mc]) / np.sum(self.AA[t, mc])  # average leverage

    def plot_results(self, variable_name):
        time_steps = np.arange(1, self.Time)
        variable_values = getattr(self, variable_name)[1:, :]
        plt.figure(figsize=(10, 6))
        for j in range(self.MC):
            plt.plot(time_steps, variable_values[:, j], label=f'Simulation {j + 1}')
        plt.xlabel('Time')
        plt.ylabel(f'{variable_name}')
        plt.title(f'{variable_name} Over Time (Multiple Simulations)')
        plt.legend()
        plt.show()

# Set parameters
Time = 1000  # number of simulation periods
Ni = 100  # number of firms
MC = 3  # number of multiple simulations
gamma = 2  # fraction of reinvested profit
phi = 0.1  # capital productivity
Pbar = 0.01  # random price drift
delta = 0.05  # depreciation rate
rbar = 0.075  # interest rate

# Create an instance of the ToyModelMC class
toy_model = MonteCarloFirmSimulation(Time, Ni, MC, gamma, phi, Pbar, delta, rbar, seed=None)

# Run the simulation
toy_model.run_simulation()

# Plot the results
toy_model.plot_results('BB')
