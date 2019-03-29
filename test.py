import numpy as np

X = np.linspace(-3, 3, 100)
y = np.sin(X)
z = np.cos(X)

plt.title("Sin vs Cos")
plt.plot(X, y, color='red', label='sin')
plt.plot(X, z, color='blue', label='cos')
plt.legend()
plt.show()