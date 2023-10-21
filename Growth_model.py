# Modelo de Crescimento Neoclássico em Tempo Finito
#################
import numpy as np

# Parâmetros do modelo
T = 50
alpha = 0.3
beta = 0.96
delta = 0.1
nk = 200

# Capital no estado estacionário
k_ss = (alpha/((1/beta)-(1-delta)))**(1/(1-alpha))

# Espaço do capital discretizado
k_min = 2 * k_ss / nk
k_max = 2 * k_ss
grid = np.linspace(k_min, k_max, nk)

# Função utilidade
def utility(c):
    return np.log(c)

# Algoritmo de iteração de valor
def value_iteration():
    V = np.zeros(nk)  # Vetor para a função valor
    g = np.zeros(nk)  # Vetor para a função política

    for t in range(T - 1, -1, -1):
        V_new = np.zeros(nk)

        for i in range(nk):
            k = grid[i]
            value = k**alpha + k*(1 - delta) - grid
            value[value <= 0] = 1e-8  # Substituindo valores inválidos por um valor pequeno positivo
            values = utility(value) + beta * V
            V_new[i] = np.max(values)
            g[i] = grid[np.argmax(values)]

        V = V_new.copy()

    return V, g

# Encontrando a função valor e a função política
V_optimal, policy = value_iteration()

# Simulação da sequência ótima de capital
k0 = k_min
capital_sequence = [k0]

for t in range(T):
    k0 = policy[np.argmin(np.abs(grid - k0))]
    capital_sequence.append(k0)

# Plot da solução
import matplotlib.pyplot as plt

time_periods = np.arange(T + 1)
plt.axhline(y=k_ss, color='red', linestyle='dashed', label="Estado Estacionário")
plt.plot(time_periods, capital_sequence)
plt.xlabel('Período')
plt.ylabel('Capital')
plt.title('Sequência Ótima de Capital ao Longo do Tempo')
plt.grid(True)
plt.show()

# c)
## Beta = 0.8
T = 50
alpha = 0.3
beta = 0.8
delta = 0.1
nk = 200

# Capital no estado estacionário
k_ss = (alpha/((1/beta)-(1-delta)))**(1/(1-alpha))

# Espaço do capital discretizado
k_min = 2 * k_ss / nk
k_max = 2 * k_ss
grid = np.linspace(k_min, k_max, nk)

V_optimal, policy = value_iteration()

# Simulação da sequência ótima de capital
k0 = k_min
capital_sequence = [k0]

for t in range(T):
    k0 = policy[np.argmin(np.abs(grid - k0))]
    capital_sequence.append(k0)

# Plot da solução
import matplotlib.pyplot as plt

plt.clf()
time_periods = np.arange(T + 1)
plt.axhline(y=k_ss, color='red', linestyle='dashed', label="Estado Estacionário")
plt.plot(time_periods, capital_sequence)
plt.xlabel('Período')
plt.ylabel('Capital')
plt.title('Sequência Ótima de Capital ao Longo do Tempo')
plt.grid(True)
plt.show()


## Beta = 0.99
T = 50
alpha = 0.3
beta = 0.99
delta = 0.1
nk = 200

# Capital no estado estacionário
k_ss = (alpha/((1/beta)-(1-delta)))**(1/(1-alpha))

# Espaço do capital discretizado
k_min = 2 * k_ss / nk
k_max = 2 * k_ss
grid = np.linspace(k_min, k_max, nk)

V_optimal, policy = value_iteration()

# Simulação da sequência ótima de capital
k0 = k_min
capital_sequence = [k0]

for t in range(T):
    k0 = policy[np.argmin(np.abs(grid - k0))]
    capital_sequence.append(k0)

# Plot da solução
import matplotlib.pyplot as plt

plt.clf()
time_periods = np.arange(T + 1)
plt.axhline(y=k_ss, color='red', linestyle='dashed', label="Estado Estacionário")
plt.plot(time_periods, capital_sequence)
plt.xlabel('Período')
plt.ylabel('Capital')
plt.title('Sequência Ótima de Capital ao Longo do Tempo')
plt.grid(True)
plt.show()


## T =  500
import numpy as np
import matplotlib.pyplot as plt

# Parâmetros do modelo
T = 500
alpha = 0.3
delta = 0.1
beta_values = [0.96]
nk = 200
tolerance = 0.0001  # Tolerância para considerar a sequência no estado estacionário

# Loop para cada valor de beta
for beta in beta_values:
    # Capital de estado estacionário em um problema horizonte infinito
    kss = (alpha/((1/beta)-(1-delta)))**(1/(1-alpha))

    # Discretização do espaço do capital
    kmin = 2 * kss / nk
    kmax = 2 * kss
    grid = np.linspace(kmin, kmax, nk)

    # Inicialização da função valor e da função política
    V = np.zeros((T+1, nk))
    g = np.zeros((T, nk), dtype=int)

    # Implementação da Programação Dinâmica
    for t in range(T-1, -1, -1):
        for i in range(nk):
            k = grid[i]
            max_val = -np.inf
            max_k0 = None

            for j in range(nk):
                k0 = grid[j]
                if k**alpha + (1 - delta)*k - k0 >= 0 and (1 - delta)*k + k**alpha - k0 > 0:
                    val = np.log(k**alpha + (1 - delta)*k - k0) + beta * V[t+1, j]

                    if val > max_val:
                        max_val = val
                        max_k0 = k0

            V[t, i] = max_val
            g[t, i] = np.where(grid == max_k0)[0][0]

    # Obtendo a política ótima de acumulação de capital
    policy_function = np.interp(grid, grid, g[0])  # Extraímos apenas a política do primeiro período

    # Simulação da sequência ótima de capital
    k0 = kmin
    k_sequence = [k0]
    num_periods_to_converge = None
    num_periods_to_deaccumulate = None

    for t in range(T):
        index = int(np.where(grid == k_sequence[t])[0])
        k_optimal_next = grid[g[t, index]]
        k_sequence.append(k_optimal_next)

        # Verificação para parar a simulação se a sequência estiver próxima do estado estacionário
        if abs(k_sequence[-1] - k_sequence[-2]) < tolerance and num_periods_to_converge is None:
            num_periods_to_converge = t

        # Verificação para parar a simulação se a sequência começar a desacumular capital
        if t > 0 and k_sequence[-1] < k_sequence[-2] and num_periods_to_deaccumulate is None:
            num_periods_to_deaccumulate = t

    # Visualizando a sequência ótima de capital
    plt.clf()
    plt.plot(range(T+1), k_sequence, label=f"β = {beta}")
    plt.axhline(y=kss, color='red', linestyle='dashed', label="Estado Estacionário")
    plt.title("Sequência Ótima de Capital para Diferentes Valores de β")
    plt.xlabel("Período")
    plt.ylabel("Capital")
    plt.legend()
    plt.show()

    # Imprimir o número de períodos para atingir o estado estacionário
    if num_periods_to_converge is not None:
        print(f"Para β = {beta}, a sequência ótima atinge o estado estacionário em {num_periods_to_converge} períodos.")
    else:
        print(f"Para β = {beta}, a sequência ótima não converge para o estado estacionário dentro do horizonte T.")

    # Imprimir o número de períodos para começar a desacumular capital
    if num_periods_to_deaccumulate is not None:
        print(f"Para β = {beta}, a sequência ótima começa a desacumular capital em {num_periods_to_deaccumulate} períodos.")
    else:
        print(f"Para β = {beta}, a sequência ótima não começa a desacumular capital dentro do horizonte T.")



# Plotar gráfico com as sequências ótimas para diferentes valores de beta



## Modelo de crescimento neoclássico estocástico
import numpy as np
import matplotlib.pyplot as plt

# Parâmetros do modelo
beta = 0.95  # Discount factor
alpha = 0.33  # Capital share in production function
delta = 0.1  # Depreciation rate
a = 0.05  # Productivity shock magnitude
p = 0.8  # Transition probability
tolerance = 1e-6  # Tolerance level for convergence
max_iterations = 5000  # Maximum number of iterations

# Discretização do espaço do capital
# Capital grid
kss = ((1-beta*(1-delta))/(alpha*beta))**(1/(alpha-1))  # Steady state capital stock
nk = 200  # Number of grid points for capital stock
k_min = 2*kss/nk  # Minimum value of capital stock
k_max = 2*kss  # Maximum value of capital stock

grid_k = np.linspace(k_min, k_max, nk)

# Estados da produtividade
A = [1 - a, 1 + a]

# Inicialização da função valor
V = np.zeros((nk, len(A)))

# Função de produção
def production_function(k, A):
    return A * k**alpha

# Função utilidade
def utility_function(c):
    return np.log(c)

# Iteração de Valor
converged = False
num_iterations = 0

while not converged and num_iterations < max_iterations:
    V_new = np.zeros_like(V)

    for i in range(nk):
        k = grid_k[i]

        for j in range(len(A)):
            A_state = A[j]

            # Encontrar o valor ótimo para consumo (k0)
            max_val = -np.inf
            max_k0 = None

            for l in range(nk):
                k0 = grid_k[l]
                c = production_function(k, A_state) + (1 - delta) * k - k0

                if c <= 0:
                    break

                val = utility_function(c) + p * V[l, 0] + (1 - p) * V[l, 1]

                if val > max_val:
                    max_val = val
                    max_k0 = k0

            V_new[i, j] = max_val

    if np.max(np.abs(V_new - V)) < tolerance:
        converged = True

    V = V_new
    num_iterations += 1

print(f"Convergência alcançada após {num_iterations} iterações.")

# Plotando as funções valor V(k, A1) e V(k, A2)
plt.plot(grid_k, V[:, 0], label="V(k, A1)", linestyle='-', color='b')
plt.plot(grid_k, V[:, 1], label="V(k, A2)", linestyle='--', color='r')
plt.xlabel("Capital (k)")
plt.ylabel("Função Valor (V)")
plt.title("Funções Valor V(k, A1) e V(k, A2)")
plt.legend()
plt.grid()
plt.show()


