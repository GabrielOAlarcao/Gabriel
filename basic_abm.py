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

    def run_simulation(self, variable_name, change_values, change_start, change_end):
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
                if change_start <= t <= change_end:
                    setattr(self, variable_name, change_values[t - change_start])
                else:
                    setattr(self, variable_name, getattr(self, f'{variable_name}_original'))
                    
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
      
        #for j in range(self.MC):
        #    plt.plot(time_steps, variable_values[:, j], label=f'Simulation {j + 1}')
        
        # Calculate median and confidence interval
        median_values = np.median(variable_values, axis=1)
        std_values = np.std(variable_values, axis=1)
        lower_ci = median_values - 2 * std_values
        upper_ci = median_values + 2 * std_values
        
        plt.figure(figsize=(10, 6))
        plt.plot(time_steps, median_values, label='Median')
        plt.fill_between(time_steps, lower_ci, upper_ci, alpha=0.3, label='Confidence Interval (2 std)')
        
        plt.xlabel('Time')
        plt.ylabel(f'{variable_name}')
        plt.title(f'{variable_name} Over Time (Multiple Simulations)')
        plt.legend()
        plt.show()
        
    def sensitivity_analysis(self, parameter_name, initial_value, perturbation, num_points, random_seed):
        # Initialize arrays to store results
        parameter_values = np.zeros(num_points)
        median_results = np.zeros((num_points, self.Time - 1))
        
        # Fixing the random values in order to isolate the parameter effect
        np.random.seed(random_seed)

        for i in range(num_points):
            # Create an instance of the MonteCarloFirmSimulation class with updated parameter
            simulation_params = {
                'Time': self.Time,
                'Ni': self.Ni,
                'MC': self.MC,
                'gamma': self.gamma,
                'phi': self.phi,
                'delta': self.delta,
                'rbar': self.rbar,
                'seed': None
            }
            simulation_params[parameter_name] = initial_value + i * perturbation

            toy_model = MonteCarloFirmSimulation(**simulation_params)

            # Run the simulation
            toy_model.run_simulation()

            # Store parameter values and median results
            parameter_values[i] = initial_value + i * perturbation
            median_results[i, :] = np.median(toy_model.BB[1:, :], axis=1)

        # Plot sensitivity analysis results
        plt.figure(figsize=(10, 6))
        for i in range(num_points):
            plt.plot(np.arange(1, self.Time), median_results[i, :], label=f'{parameter_name} = {parameter_values[i]}')

        plt.xlabel('Time')
        plt.ylabel('BB')
        plt.title(f'Sensitivity Analysis on {parameter_name}')
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

# Plot the results ('YY', 'AA', 'BB' or 'LEV')
toy_model.plot_results('BB')

# Performing sensitivity analysis 
toy_model.sensitivity_analysis(parameter_name='Pbar', initial_value=0.01, perturbation=0.005, num_points=5, random_seed = 15)

# Policy Analysis (Change in rbar from 0.075 to 0.1 after t > 500)
toy_model.run_simulation('rbar', change_values={t: 0.1 for t in range(501, 1001)}, change_start=501, change_end=1000)
