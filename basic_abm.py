## Simulation Model
# Economics with Heterogeneous Interacting Agents
import numpy as np
import matplotlib.pyplot as plt

class FirmSimulation:
    def __init__(self, Ni, Time, gamma, phi, Pbar, delta, rbar, MC):
        self.Ni = Ni
        self.Time = Time
        self.gamma = gamma
        self.phi = phi
        self.Pbar = Pbar
        self.delta = delta
        self.rbar = rbar
        self.MC = MC
        self.r = np.ones((Ni, Time, MC))
        self.A = np.ones((Ni, Time, MC))
        self.K = np.ones((Ni, Time, MC))
        self.B = np.zeros((Ni, Time, MC))
        self.I = np.zeros((Ni, Time, MC))
        self.P = np.zeros((Ni, Time, MC))
        self.Y = np.zeros((Ni, Time, MC))
        self.Z = 2 * np.random.rand(Ni, 1, MC) + Pbar
        self.YY = np.zeros((Time, MC))
        self.AA = np.zeros((Time, MC))
        self.BB = np.zeros((Time, MC))

    def run_simulation(self):
        for mc in range(self.MC):
            for t in range(1, self.Time):
                self.I[:, t, mc] = self.gamma * self.Z[:, t-1, mc]
                self.K[:, t, mc] = (1 - self.delta) * self.K[:, t-1, mc] + self.I[:, t, mc]
                self.Y[:, t, mc] = self.phi * self.K[:, t, mc]
                self.B[:, t, mc] = self.K[:, t, mc] - self.A[:, t-1, mc]
                self.B[self.B[:, t, mc] < 0, t, mc] = 0
                self.P[:, t, mc] = 2 * np.random.rand(self.Ni) + self.Pbar
                self.Z[:, t, mc] = self.P[:, t, mc] * self.Y[:, t, mc] - self.r[:, t, mc] * self.K[:, t, mc]
                self.A[:, t, mc] = self.A[:, t-1, mc] + self.Z[:, t, mc]
                self.r[:, t, mc] = self.rbar + self.rbar * (self.B[:, t, mc] / self.A[:, t, mc]) ** self.rbar
                self.Z[self.A[:, t, mc] < 0, t, mc] = 0
                self.I[self.I[:, t, mc] < 0, t, mc] = 0
                self.K[self.A[:, t, mc] < 0, t, mc] = 1
                self.A[self.A[:, t, mc] < 0, t, mc] = 1
                self.YY[t, mc] = np.sum(self.Y[:, t, mc])
                self.AA[t, mc] = np.sum(self.A[:, t, mc])
                self.BB[t, mc] = np.sum(self.B[:, t, mc])

    def plot_results(self, variable_name, simulation_index=0):
        time_steps = np.arange(1, self.Time)
        variable_values = getattr(self, variable_name)[1:, simulation_index]

        plt.figure(figsize=(10, 4))

        for mc in range(self.MC):
          plt.plot(time_steps, variable_values[:, mc], label=f'{variable_name} (Simulation {mc + 1})')
          
        plt.title(f'{variable_name} Over Time (All Simulations)')
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
rbar = 0.075  # Interest rate
delta = 0.05 # Depreciation rate

simulation = FirmSimulation(Ni, Time, gamma, phi, Pbar, rbar, delta)
simulation.run_simulation()

# Plotting different variables
simulation.plot_results('YY')  # Aggregate Production
simulation.plot_results('AA')   # Net Worth
simulation.plot_results('BB')   # Capital
# ... Add more as needed
