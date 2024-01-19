## Simulation Model
import numpy as np
import matplotlib.pyplot as plt

class FirmSimulation:
    def __init__(self, Ni, Time, gamma, phi, Pbar, r, delta, rbar):
        self.Ni = Ni
        self.Time = Time
        self.gamma = gamma
        self.phi = phi
        self.Pbar = Pbar
        self.r = r
        self.delta = delta
        self.rbar = rbar
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
            self.K = (1 - self.delta)*self.K + self.I
            self.Y = self.phi * self.K
            self.B = self.K - self.A
            self.B[self.B < 0] = 0
            self.P = 2 * np.random.rand(self.Ni, 1) + self.Pbar
            self.Z = self.P * self.Y - self.r * self.K
            self.A = self.A + self.Z
            self.r = self.rbar + self.rbar*(self.B/self.A)**(self.rbar)
            self.Z[self.A < 0] = 0
            self.I[self.I < 0] = 0
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
phi = 0.1  # Capital produtivity
Pbar = 0.01  # Price constant
r = 0.1  # Interest rate
delta = 0.05 # Depreciation rate

simulation = FirmSimulation(Ni, Time, gamma, phi, Pbar, r)
simulation.run_simulation()

# Plotting different variables
simulation.plot_results('YY')  # Aggregate Production
simulation.plot_results('AA')   # Net Worth
simulation.plot_results('BB')   # Capital
# ... Add more as needed
