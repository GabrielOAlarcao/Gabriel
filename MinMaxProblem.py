## Minimum Maximum Problem (MINMAX)
# Given an acyclic graph with weighted branches, the shortest path problem is
# to find the path from source s to target t whose (weighted) length is minimal.
# Each path is a sequence of branches, and its length is the sum of the weights
# of the branches in the path. The maximal link in any path is the maximum
# of the weights of the branches in the path. The MINMAX problem is that of
# finding the path from s to t whose maximal link is minimal. To use DP to solve
# this problem, ordinarily we would construct the MINMAX path by making a
# sequence of decisions for the branches in the path, and attribute to each of
# these decisions a separable cost. However, for this problem these cost are not
# additive (or multiplicative, as for RDP), as is usually the case. Despite this,
# the problem can still be solved using DP.

###############
def minmax_path(graph, source, target):
    # Initialize DP table
    dp = {}
    maxlink = {}
    
    # Helper function to calculate the maximal link in a path
    def calculate_maxlink(path):
        return max(graph[node][next_node] for node, next_node in zip(path[:-1], path[1:]))
        
    # Base case: when the path reaches the target, return the maximal link
    def base_case(path, last_node):
        if last_node == target:
            maxlink[(frozenset(path), last_node)] = calculate_maxlink(path)
            
    # Recursive DP function
    def dp_function(path, last_node):
        # Check if the subproblem has already been solved
        if (frozenset(path), last_node) in dp:
            return dp[(frozenset(path), last_node)]
            
        # Base case
        base_case(path, last_node)
        
        # Recursive case
        if last_node == target:
            return maxlink[(frozenset(path), last_node)], path
            
        # Calculate the DP value
        minmax = float('inf')
        minmax_path = []
        
        # Check if there are neighbors for the current node
        if last_node in graph:
            for next_node in graph[last_node]:
                new_path = path + [next_node]
                cost = calculate_maxlink(new_path) if len(new_path) == len(graph) - 1 else 0
                value, subpath = dp_function(new_path, next_node)
                value = max(value, cost)
                
                if graph[last_node][next_node] < minmax:
                    minmax = graph[last_node][next_node]
                    minmax_path = subpath  # Update the minmax_path
        
        # Update the last_node
        last_node = minmax_path[-1] if minmax_path else last_node
        
        # Memoize the result and return
        dp[(frozenset(path), last_node)] = minmax, minmax_path
        return minmax, minmax_path
        
    # Start the DP recursion
    minmax, path = dp_function([source], source)
    minmax = calculate_maxlink(path)
    return minmax, path
  
if __name__ == '__main__':
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
