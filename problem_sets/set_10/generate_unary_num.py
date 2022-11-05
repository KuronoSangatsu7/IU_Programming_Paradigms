limit = input()
limit = int(limit)

cur = 'z'

for i in range(limit):
    cur = 's(' + cur + ')'

print(cur)