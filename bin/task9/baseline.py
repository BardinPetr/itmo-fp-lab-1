def search(sum=1000):
  for a in range(1, sum):
    for b in range(a + 1, sum):
      c = sum - a - b
      if a**2 + b**2 == c**2:
        return a * b * c
        
