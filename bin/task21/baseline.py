from tqdm import tqdm

def d(n):
  return sum(i for i in range(1, n) if n % i == 0)

mx = 10000
ds = [d(i) for i in tqdm(range(mx))]

out = 0 
for i in range(1, mx):
  j = ds[i]
  if j != i and j <= mx and i == ds[j]:
    out += i

print(out)
