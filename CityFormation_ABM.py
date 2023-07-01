## City Formation Problem
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
