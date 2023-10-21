### Exercícios Métodos Computacionais ###
def euclidean_algorithm(x, y):
    """
    Função recursiva que implementa o algoritmo de Euclides para encontrar o máximo divisor comum (MDC)
    de dois números x e y.

    Args:
        x (int): Primeiro número
        y (int): Segundo número

    Returns:
        int: Máximo divisor comum (MDC) de x e y
    """
    # Caso base: quando y é igual a 0, o MDC é x
    if y == 0:
        return x
    else:
        # Chamada recursiva, passando y como novo x e o resto da divisão de x por y como novo y
        return euclidean_algorithm(y, x % y)

# Exemplo de uso:
x = 36
y = 5
mdc = euclidean_algorithm(x, y)
print(f'O máximo divisor comum de {x} e {y} é {mdc}.')


## Palíndromo
def is_palindrome(num):
    """
    Função recursiva que verifica se um número é um palíndromo.

    Args:
        num (int): Número a ser verificado

    Returns:
        bool: True se o número é um palíndromo, False caso contrário
    """
    # Converte o número para uma string para facilitar a comparação de caracteres
    num_str = str(num)

    # Caso base: se o número possui apenas 1 dígito ou é igual a 0, é um palíndromo
    if len(num_str) <= 1:
        return True
    else:
        # Verifica se o primeiro e o último dígito são iguais
        if num_str[0] == num_str[-1]:
            # Chama recursivamente a função com o número sem o primeiro e último dígito
            return is_palindrome(num_str[1:-1])
        else:
            return False
          
          
## Compute the derivative of a polynomial
# A Primer on scientific programming with Python
# Exercise 6.17

def diff(p):
    """
    Compute the derivative of a polynomial represented as a dictionary.

    Args:
        p (dict): A dictionary representing the polynomial, where the keys are the powers of x and the values are the coefficients.

    Returns:
        dict: A dictionary representing the derivative of the polynomial.

    """
    dp = {}  # Initialize the dictionary for the derivative
    for j in p:
        if j == 0:
            continue  # Skip the constant term, as its derivative is 0
        dp[j - 1] = j * p[j]  # Compute the derivative of the term
    return dp
  
p = {2: 1, 5: -1, 6 : 2}  

diff(p)


# Compute the area of a triangle
# A Primer on scientific programming with Python
# Exercise 6.15
def area_triangle(x):
  x_coord = []
  y_coord = []
  
  for j in x:
    x_coord.append(x[j][0])
    y_coord.append(x[j][1])
    
    area = (sum(x_coord) * sum(y_coord))/2
    
  return area

area = {1: (4,0), 2: (1,0), 3: (0,2)}

area_triangle(area)
  
  
# Sum of the first n squared integers
# Introduction to recursive programming
# Exercise 3.6

def sum_of_squares_recursive(n):
    """
    Calculates the sum of the first n squared integers using recursion.
    
    Args:
        n (int): Number of terms
        
    Returns:
        int: Sum of the first n squared integers
    """
    if n == 1:
        return 1
    else:
        return n**2 + sum_of_squares_recursive(n - 1)
      
# Introduction to recursive programming
# Exercise 3.18 (a)
def relations_a(n):
  if n == 0:
    return 0
  else:
    return 2* relations_a(n-1) + 3*n - 2
  
  
# Introduction to recursive programming
# Exercise 3.18 (b)
def is_power_of_2(n):
    # Base case: n should be greater than 0
    if n <= 0:
        return False
    
    # Keep dividing n by 2 until it becomes 1
    while n > 1:
        # If n is not divisible by 2, it is not a power of 2
        if n % 2 != 0:
            return False
        n = n // 2
    
    # If n becomes 1, it is a power of 2
    return True

def T(n):
    """
    Calculates the value of T(n) for the given expression: T(n) = T(n/2) + n
    
    Args:
        n (int): Input value
        
    Returns:
        int: Value of T(n)
    """
    if n == 1:
        return 1
    elif is_power_of_2(n) == True:
        return T(n//2) + n 
    else:
      return print("n in not power of 2")

T(8)


## Make a class for quadratic functions.
# A Primer on scientific programming with Python
# Exercise 7.7
class Quadratic:
    def __init__(self, a, b, c):
        self.a = a
        self.b = b
        self.c = c
    
    def value(self, x):
        return self.a * x**2 + self.b * x + self.c
    
    def table(self, L, R, n):
        dx = (R - L) / (n - 1)
        x_values = [L + i * dx for i in range(n)]
        f_values = [self.value(x) for x in x_values]
        table = [(x_values[i], f_values[i]) for i in range(n)]
        return table
    
    def roots(self):
        delta = self.b**2 - 4 * self.a * self.c
        if delta < 0:
            return None
        elif delta == 0:
            return -self.b / (2 * self.a)
        else:
            root1 = (-self.b + delta**0.5) / (2 * self.a)
            root2 = (-self.b - delta**0.5) / (2 * self.a)
            return root1, root2
          

## Make a class for straight lines.
# A Primer on scientific programming with Python
# Exercise 7.5

class Line:
  def __init__(self, p1, p2):
    self.p1 = p1
    self.p2 = p2
  
  def slope(self):
    x1, y1 = self.p1
    x2, y2 = self.p2
    
    return (y2 - y1)/(x2 - x1)
  
  def intercept(self):
    x1, y1 = self.p1
    a = self.slope()
    
    return y1 - a * x1
  
  def value(self, x):
    a = self.slope()
    b = self.intercept()
    
    return a * x + b
    
    
line = Line((0,-1), (2,4))
print(line.value(0.5))

# Create a function transpose that transposes matrices (lists of lists).
#https://gist.github.com/oskarkv/3168ea3f8d7530ccd94c97c19aafe266

def transpose(matriz):
  """
  Returns the transpose of the given matrix (a list of lists).
    
  """
  return [[matriz[j][i] for j in range(len(matriz))] for i in range(len(matriz[0]))]


# Create a function interleave take takes an arbitrary number of sequences and interleaves them.
#https://gist.github.com/oskarkv/3168ea3f8d7530ccd94c97c19aafe266
def interleave(*sequences):
  """
  Interleaves an arbitrary number of sequences (lists, tuples, etc.).
    
  """
  max_len = max(len(seq) for seq in sequences)
  result = []
  for i in range(max_len):
    for seq in sequences:
      if i < len(seq):
        result.append(seq[i])
        
  return result      
        
  
interleave([1, 2, 3], [10, 20, 30], "abc")

# Create a function zipmap that takes a two seqences, and creates a dictionary from the 
# elements of the first seqence to the elements of the second.
# https://gist.github.com/oskarkv/3168ea3f8d7530ccd94c97c19aafe266

def zipmap(keys, values):
  """
  Takes two sequences, and creates a dictionary from the elements of the first sequence to the elements of the second.
    
  """
  result = {}
  for i in range(len(keys)):
    result[keys[i]] = values[i]
    
  return result 


# Create a function zipmap that takes a two seqences, and creates a dictionary from the 
# elements of the first seqence to the elements of the second.
# https://gist.github.com/oskarkv/3168ea3f8d7530ccd94c97c19aafe266

def zipmap_2(keys, *values):
  """
  Takes sequences, and creates a dictionary from the elements of the first sequence to the elements of the second.
    
  """
  result = {}
  for i in range(len(keys)):
    for value in values:
      result[keys[i]] = [value[i] for value in values]
    
  return result 

a = ["a","b","c"]
b = [10,20,30]
c = [100,200,300]

print(zipmap([1,2,3],[4,5,6]))

print(zipmap_2(["a","b","c"],[1,2,3],[4,5,6]))

# Exercıcio 8 do capítulo 11 do livro Combinatorics:  Topics, Techniques, Algorithms by Peter J.Cameron
# Choose ten towns in your country. Find from Atlas (or estimate) the distance between all pairs of towns. Then
# a) find a minimal connector
# b) Use the 'twice-round-the-tree' algorithm to find a route for the Traveling Salesman

import math
import itertools

def euclidean_distance(city1, city2):
    lat1, lon1 = city1
    lat2, lon2 = city2
    return math.sqrt((lat1 - lat2) ** 2 + (lon1 - lon2) ** 2)

def tsp_dynamic_programming(cities):
    num_cities = len(cities)
    all_cities = list(cities.keys())
    dp = {}

    # Helper function to compute the optimal path using dynamic programming
    def compute_path(mask, last_city):
        if mask == (1 << num_cities) - 1:
            return euclidean_distance(cities[last_city], cities[all_cities[0]]), [all_cities[0]]

        if (mask, last_city) in dp:
            return dp[(mask, last_city)]

        min_distance = float('inf')
        optimal_path = []

        for next_city in range(num_cities):
            if (mask >> next_city) & 1 == 0:
                distance = euclidean_distance(cities[last_city], cities[all_cities[next_city]])
                new_distance, new_path = compute_path(mask | (1 << next_city), all_cities[next_city])
                new_distance += distance

                if new_distance < min_distance:
                    min_distance = new_distance
                    optimal_path = [all_cities[next_city]] + new_path

        dp[(mask, last_city)] = min_distance, optimal_path
        return min_distance, optimal_path

    _, optimal_path = compute_path(1 << 0, all_cities[0])
    return optimal_path

# Define the coordinates of the cities
cities = {
    "Londres": (51.5074, -0.1278),
    "Liverpool": (53.4084, -2.9916),
    "Machester": (53.4831, -2.2441),
    "York": (53.96142, -1.07391),
    "Bath": (51.3758, -2.3599),
    "Edimburgo": (55.9533, -3.1883),
    "Glasgow": (55.8642, -4.2518),
    "Inverness": (57.4778, -4.2247),
    "Dublin": (53.3498, -6.2603),
    "Cork": (51.8969, -8.4863),
    "Belfast": (54.5973, -5.9301)
}

# Find the optimal path
optimal_path = tsp_dynamic_programming(cities)

# Print the optimal path
print("Optimal Path:")
for city in optimal_path:
    print(city)

# Exercício 2.5 do livro Dynamic Programming: A computational tool - Art Lew e Holger Mauch
# Optimal Assignment Problem (ASSIGN)

def solve_matching_problem(A, B, cost):
    n = len(A)
    f = {}  # Dictionary to store the computed values of f(k, S)

    def dp(k, S):
        if len(S) == 0:  # Base case: All members of A have been assigned
            return 0

        state = (k, tuple(sorted(S)))  # Convert the set S to a tuple to use it as a key in the dictionary

        if state in f:  # Return the pre-computed value if available
            return f[state]

        min_cost = float('inf')  # Initialize the minimum cost with infinity

        for d in S:
            new_S = S - {d}  # Compute the next state by removing the assigned member from the set

            # Recursive step: Compute the cost of assigning aj to bi and add it to the cost of the next state
            current_cost = cost(A[k], B[d]) + dp(k + 1, new_S)

            min_cost = min(min_cost, current_cost)  # Update the minimum cost

        f[state] = min_cost  # Store the computed value for future reference
        return min_cost

    # Call the dynamic programming function with initial state (0, A)
    return dp(0, set(range(n)))


A = [1, 2, 3]
B = [4, 5, 6]
cost = lambda a, b: abs(a - b)  # Corrected cost function

minimum_cost = solve_matching_problem(A, B, cost)
print("Minimum cost:", minimum_cost)

## Monte Carlo
# Exercício 2.1 do livro Monte Carlo Statistical Methods by Christian Robert and George Casella
# a) Generate 1000 uniform random variables and make a histogram

import numpy as np
import matplotlib.pyplot as plt

# Generate 1000 uniform random variables between 0 and 1
random_vars = np.random.uniform(0, 1, 1000)

# Create a histogram with 10 bins
plt.hist(random_vars, bins=10)

# Add labels and title
plt.xlabel('Value')
plt.ylabel('Frequency')
plt.title('Uniform Random Variables Histogram')

# Display the histogram
plt.show()


# b) Generate uniform random variables and plot the pairs (Xi, Xi+1) to check for autocorrelation

# Set the seed for reproducibility (optional)
np.random.seed(42)

# Define the number of random variables
N = 1000

# Generate random variables from a standard normal distribution
random_vars = np.random.randn(N)

# Plot the pairs (Xi, Xi+1)
plt.scatter(random_vars[:-1], random_vars[1:])

# Add labels and title
plt.xlabel('Xi')
plt.ylabel('Xi+1')
plt.title('Autocorrelation Plot')

# Display the plot
plt.show()


## Exercício 3.10 do livro Scientific Computing:  An Introductory Survey, 2018 - Michael T.Heath
# a)
import numpy as np

def check_inverse_equality(A):
    # Compute A^T * A
    ATA = np.dot(A.T, A)

    # Compute the inverse of ATA
    ATA_inv = np.linalg.inv(ATA)

    # Perform QR factorization of A
    Q, R = np.linalg.qr(A)

    # Compute R^T * R
    RT_R = np.dot(R.T, R)

    # Compute the inverse of RT_R
    RTR_inv = np.linalg.inv(RT_R)

    # Check if the two inverses are equal
    are_equal = np.allclose(ATA_inv, RTR_inv)

    # Print the result
    if are_equal:
        print("(A^T * A)^(-1) is equal to (R^T * R)^(-1)")
    else:
        print("(A^T * A)^(-1) is not equal to (R^T * R)^(-1)")

# Define a matrix A
A = np.array([[4, 2, 3], [1, 5, 8], [7, 6, 9]])

# Call the function with matrix A
check_inverse_equality(A)



# b) 
num_matrices = 10

# Create an empty list to store the matrices
matrices = []

# Generate and save the matrices
for i in range(num_matrices):
    # Generate a random matrix of size 3x3
    matrix = np.random.rand(3, 3)
    matrices.append(matrix)
    
# for i, matrix in enumerate(matrices):
#   ATA = np.dot(matrix.T, matrix)
# 
#   # Compute the inverse of ATA
#   ATA_inv = np.linalg.inv(ATA)
# 
#   # Perform QR factorization of A
#   Q, R = np.linalg.qr(matrix)
# 
#   # Compute R^T * R
#   RT_R = np.dot(R.T, R)
# 
#   # Compute the inverse of RT_R
#   RTR_inv = np.linalg.inv(RT_R)
# 
#   # Check if the two inverses are equal
#   are_equal = np.allclose(ATA_inv, RTR_inv)
# 
#   # Print the result
#   if are_equal:
#       print("(A^T * A)^(-1) is equal to (R^T * R)^(-1)")
#   else:
#       print("(A^T * A)^(-1) is not equal to (R^T * R)^(-1)")


def RTR_inversa(A):
  Q, R = np.linalg.qr(A)
  RT_R = np.dot(R.T, R)
  RTR_inv = np.linalg.inv(RT_R)
  
  return RTR_inv

def checar_igualdade(A, RTR_inv):
  ATA = np.dot(matrix.T, matrix)

  # Compute the inverse of ATA
  ATA_inv = np.linalg.inv(ATA)
  
  are_equal = np.allclose(ATA_inv, RTR_inv)

  # Print the result
  if are_equal:
      print("(A^T * A)^(-1) is equal to (R^T * R)^(-1)")
  else:
      print("(A^T * A)^(-1) is not equal to (R^T * R)^(-1)")
  
for i, matrix in enumerate(matrices):
  RTR_inv = RTR_inversa(matrix)
  
  checar_igualdade(matrix, RTR_inv)

## Exercício 2.28 do livro Dynamic Programming:  A computational tool – Art Lew e Holger Mauch. 
## Minimum Maximum Problem (MINMAX)
###############
def minmax_path(graph, source, target):
    # Inicializa a tabela da PD
    dp = {}
    maxlink = {}
    
    # Função auxiliar para calcular o link máximo no caminho
    def calculate_maxlink(path):
        return max(graph[node][next_node] for node, next_node in zip(path[:-1], path[1:]))
        
    # Caso base: quando o caminho atinge o alvo, retorna o link máximo
    def base_case(path, last_node):
        if last_node == target:
            maxlink[(frozenset(path), last_node)] = calculate_maxlink(path)
            
    # Função recursiva
    def dp_function(path, last_node):
        # Confere se o subproblema já foi resolvido
        if (frozenset(path), last_node) in dp:
            return dp[(frozenset(path), last_node)]
            
        # Caso base
        base_case(path, last_node)
        
        # Caso recursivo
        if last_node == target:
            return maxlink[(frozenset(path), last_node)], path
            
        # Calcula o caminho
        minmax = float('inf')
        minmax_path = []
        
        # Confere se tem algum vizinho para o nó atual
        if last_node in graph:
            for next_node in graph[last_node]:
                new_path = path + [next_node]
                cost = calculate_maxlink(new_path) if len(new_path) == len(graph) - 1 else 0
                value, subpath = dp_function(new_path, next_node)
                value = max(value, cost)
                
                if graph[last_node][next_node] < minmax:
                    minmax = graph[last_node][next_node]
                    minmax_path = subpath  # Atualiza o minmax_path
        
        # Atualiza o last_node
        last_node = minmax_path[-1] if minmax_path else last_node
        
        # memoriza o resultado e retorna
        dp[(frozenset(path), last_node)] = minmax, minmax_path
        return minmax, minmax_path
        
    # Inicia a programação dinâmica recursiva
    minmax, path = dp_function([source], source)
    minmax = calculate_maxlink(path)
    return minmax, path

# Example usage:
graph = {
    '0': {'1': 10, '2': 7, '3': 6},
    '1': {'4': 9},
    '2': {'4': 7},
    '3': {'4': 11, '5': 7},
    '4': {'6': 8, '7': 7, '8': 10},
    '5': {'6': 8, '7': 6, '8': 7},
    '6': {'9': 13},
    '7': {'9': 8},
    '8': {'9': 9},
    '9': {}
}

source_node = '0'
target_node = '9'

result_minmax, result_path = minmax_path(graph, source_node, target_node)
print("Shortest path with minimal maximal link:", result_minmax)
print("Path:", result_path)


# Flatten
def flatten(tree):
    return [item for sublist in tree for item in (flatten(sublist) if isinstance(sublist, list) else [sublist])]

# Example usage:
tree = [1, [2, [3, 4], [5, 6], 7], 8, [9, 10]]
flattened_tree = flatten(tree)
print(f'Método 2: {flattened_tree}')


### City Formation Problem
import random
import numpy as np
import matplotlib.pyplot as plt

# Parameters
num_agents = 1000
num_sites = 100
neighborhood_size = 1
max_iterations = 100

# Initialize agents and sites
initial_agents = [random.randint(0, num_sites-1) for _ in range(num_agents)]
agents = initial_agents.copy()
site_populations = [initial_agents.count(site) for site in range(num_sites)]

# Simulation loop
for _ in range(max_iterations):
    # Shuffle the order in which agents act
    random.shuffle(agents)

    # Update agent locations
    for agent in agents:
        current_site = agents[agent]
        current_population = site_populations[current_site]

        # Evaluate potential new locations
        neighborhood = list(range(max(0, current_site-neighborhood_size), min(num_sites, current_site+neighborhood_size+1)))
        new_locations = [site for site in neighborhood if site != current_site]
        best_location = current_site
        best_score = current_population / np.mean([abs(current_site - site) for site in neighborhood])

        for location in new_locations:
            location_population = site_populations[location]
            distance = abs(current_site - location)
            score = location_population / distance

            if score > best_score:
                best_location = location
                best_score = score

        # Move agent to the best location
        agents[agent] = best_location
        site_populations[current_site] -= 1
        site_populations[best_location] += 1

# Calculate final agent counts
final_counts = [site_populations[i] for i in range(num_sites)]

# Plotting the initial and final distributions of agents
site_indices = np.arange(num_sites)
initial_counts = [initial_agents.count(site) for site in site_indices]

fig, (ax1, ax2) = plt.subplots(2, 1, sharex=True, figsize=(8, 6))
ax1.bar(site_indices, initial_counts)
ax1.set_ylabel('Agent Count (Initial)')
ax1.set_title('Distribution of Agents')

ax2.bar(site_indices, final_counts)
ax2.set_xlabel('Site')
ax2.set_ylabel('Agent Count (Final)')

plt.tight_layout()
plt.show()


## Agents dislike each other
import random
import numpy as np
import matplotlib.pyplot as plt

# Parameters
num_agents = 1000
num_sites = 100
neighborhood_size = 1
max_iterations = 100

# Initialize agents and sites
initial_agents = np.random.choice(range(num_sites), size=num_agents, replace=True, p=None)  # Agents distributed according to some distribution
agents = initial_agents.copy()
site_populations = [initial_agents.tolist().count(site) for site in range(num_sites)]

# Simulation loop
for _ in range(max_iterations):
    # Shuffle the order in which agents act
    random.shuffle(agents)

    # Update agent locations
    for agent in agents:
        current_site = agents[agent]
        current_population = site_populations[current_site]

        # Evaluate potential new locations
        neighborhood = list(range(max(0, current_site-neighborhood_size), min(num_sites, current_site+neighborhood_size+1)))
        new_locations = [site for site in neighborhood if site != current_site]

        # Check the relative number of agents on each side
        left_agents = sum(site_populations[l] for l in range(current_site))
        right_agents = sum(site_populations[r] for r in range(current_site + 1, num_sites))

        if right_agents > left_agents:
            best_location = current_site - 1 if current_site > 0 else current_site
        else:
            best_location = current_site + 1 if current_site < num_sites - 1 else current_site

        # Move agent to the best location
        agents[agent] = best_location
        site_populations[current_site] -= 1
        site_populations[best_location] += 1

# Calculate final agent counts
final_counts = [site_populations[i] for i in range(num_sites)]

# Plotting the initial and final distributions of agents
site_indices = np.arange(num_sites)
initial_counts = [initial_agents.tolist().count(site) for site in site_indices]

fig, (ax1, ax2) = plt.subplots(2, 1, sharex=True, figsize=(8, 6))
ax1.bar(site_indices, initial_counts)
ax1.set_ylabel('Agent Count (Initial)')
ax1.set_title('Distribution of Agents')

ax2.bar(site_indices, final_counts)
ax2.set_xlabel('Site')
ax2.set_ylabel('Agent Count (Final)')

plt.tight_layout()
plt.show()


###################
def balanced(s):
    stack = []
    opening_markers = ['(', '{', '[']
    closing_markers = [')', '}', ']']
    
    for char in s:
        if char in opening_markers:
            stack.append(char)
        elif char in closing_markers:
            if len(stack) == 0:
                return False
            opening_char = stack.pop()
            if opening_markers.index(opening_char) != closing_markers.index(char):
                return False
    
    return len(stack) == 0

if __name__ == "__main__":
  print(balanced("a(b)"))



 #######################
import numpy as np
import scipy
from scipy.stats import poisson
import matplotlib.pyplot as plt
from matplotlib.patches import Polygon

def generate_random_points(n):
    # Cria uma lista de "n" pontos aleatórios distribuídos uniformemente no quadrado unitário
    points = np.random.rand(n, 2)
    return points

def generate_rectangle():
    # Gera um dicionário contendo coordenadas dos vértices e áreas de um retângulo aleatorio gerado dentro do quadrado unitário
    v1 = [np.random.uniform(0,1), np.random.uniform(0,1)]
    largura = np.random.uniform(0, 1-v1[0])
    altura = np.random.uniform(0, 1-v1[1])
    v2, v3, v4 = v1.copy(), v1.copy(), v1.copy()
    v2[0] = v1[0] + largura
    v3[0], v3[1] = v1[0] + largura, v1[1] + altura
    v4[1] = v1[1] + altura
    coordenadas = [v1, v2, v3, v4]
    area = largura*altura
    R = {"vertices": coordenadas, "area": area}
    print("Retângulo R aleatório gerado:", R)
    return R

def plot_rectangle(vertices):
    # Cria uma figura e um único subplot (axes)
    fig, ax = plt.subplots()

    # Cria um polígono com base nas coordenadas dos vértices
    polygon = Polygon(vertices, closed=True, fill=False, color='red')

    # Adiciona o polígono ao subplot
    ax.add_patch(polygon)

    # Define os limites dos eixos
    ax.set_xlim([0, 1])
    ax.set_ylim([0, 1])
    ax.set_box_aspect(1)

    # Mostra a figura
    plt.show()

def plot_points(points):
    # Cria uma figura e um único subplot (axes)
    fig, ax = plt.subplots()

    # Extrai as coordenadas x e y dos pontos
    x_coords, y_coords = zip(*points)

    # Plota os pontos
    ax.scatter(x_coords, y_coords, color='blue')

    # Define os limites dos eixos
    ax.set_xlim([0, 1])
    ax.set_ylim([0, 1])
    ax.set_box_aspect(1)

    # Mostra a figura
    plt.show()

def points_inside(R, points):

    inside_points = 0
    vertices = R.get("vertices")

    for i in points:
        if vertices[0][0] <= i[0] <= vertices[1][0] and vertices[0][1] <= i[1] <= vertices[2][1]:
            inside_points += 1

    return inside_points

def repeated_experiment():

    # Gera um retângulo aleatório se nao for fornecido algum
    R = generate_rectangle()
    area = R.get("area")

    # Repetição do experimento e criação de uma lista com os resultados da proporção de pontos dentro do retângulo
    results = []
    for i in range(1000):
        points = generate_random_points(1000)
        if i == 0:
            plot_sample(R, points)
        inside_points = points_inside(R, points)
        results.append(inside_points)
    print("Pontos dentro do retângulo em cada repetição do experimento: ", results)
    print("Frequência média do experimento: ", sum(results)/1000)
    print("Média da Distribuição de Poisson: ", area*1000)
    
    plt.clf()

    # Plotar o histograma dos resultados
    plt.hist(results, bins='auto', density=True, alpha=0.7, label='Dados')

    # Calcular os parâmetros da distribuição de Poisson
    poisson_lambda = area * 1000
    x = np.arange(0, 1001)
    y = np.random.poisson(poisson_lambda, len(x))
    poisson_probs = poisson.pmf(x, poisson_lambda)

    # Plotar a distribuição de Poisson
    plt.plot(x, poisson_probs, 'r', marker='o', linestyle='-', label='Poisson')

    # Configurar o gráfico
    plt.xlabel('Número de pontos dentro do retângulo')
    plt.ylabel('Probabilidade / Frequência no experimento')
    plt.legend()
    plt.title('Comparação com Distribuição de Poisson')
    plt.show()

def plot_sample(R, points):
    # Cria uma figura e um único subplot (axes)
    fig, ax = plt.subplots()

    # Extrai as coordenadas x e y dos pontos
    x_coords, y_coords = zip(*points)

    # Plota os pontos
    ax.scatter(x_coords, y_coords, color='blue', s=1)

    # Cria um polígono com base nas coordenadas dos vértices
    polygon = Polygon(R.get("vertices"), closed=True, fill=False, color='red')

    # Adiciona o polígono ao subplot
    ax.add_patch(polygon)

    # Define os limites dos eixos
    ax.set_xlim([0, 1])
    ax.set_ylim([0, 1])
    ax.set_box_aspect(1)

    # Mostra a figura
    plt.show()
    

if __name__ == "__main__":

    repeated_experiment()
    
    
###
# Example data
from scipy import stats
data = [4, 5, 6, 7, 8, 9]

# Confidence level (e.g., 95%)
confidence_level = 0.95

# Calculate confidence interval
interval = stats.t.interval(confidence_level, len(data)-1, loc=np.mean(data), scale=stats.sem(data))

# Print the confidence interval
print("Confidence Interval:", interval)


######
import numpy as np
import matplotlib.pyplot as plt

# Número de amostras
num_samples = 10000

# Tamanhos das amostras
sample_sizes = [12, 48, 96]

# Lista para armazenar as variáveis aleatórias simuladas
simulated_samples = []

# Simular variáveis aleatórias para cada tamanho de amostra
for n in sample_sizes:
    # Gerar variáveis aleatórias uniformes
    uniform_samples = np.random.uniform(0, 1, size=(num_samples, n))

    # Calcular a média das variáveis aleatórias uniformes
    sample_mean = np.mean(uniform_samples, axis=1)

    # Ajustar a média e o desvio padrão para obter variáveis aleatórias com distribuição normal
    simulated_samples.append((sample_mean - 0.5) * np.sqrt(12 * n))

# Plotar histogramas das variáveis aleatórias simuladas
colors = ['blue', 'green', 'yellow']
labels = ['n = 12', 'n = 48', 'n = 96']

plt.figure(figsize=(10, 6))

for i in range(len(sample_sizes)):
    plt.hist(simulated_samples[i], bins=30, density=True, alpha=0.5, color=colors[i], label=labels[i])
    print(np.mean(simulated_samples[i]))
    print(np.var(simulated_samples[i]))
    # Gerar pontos para plotar a curva da distribuição normal
    x = np.linspace(-4, 4, 100)
    y = (1 / np.sqrt(2 * np.pi)) * np.exp(-(x ** 2) / 2)

    # Plotar a curva da distribuição normal
    plt.plot(x, y, color='black', linewidth=2)

    # Configurações do gráfico
    plt.xlabel('Valor')
    plt.ylabel('Densidade')
    plt.title('Simulação de Variáveis Aleatórias N(0, 1) pelo Teorema do Limite Central')
    plt.legend()
    plt.grid(True)

    # Exibir o gráfico
    plt.show()
    plt.clf()


    
