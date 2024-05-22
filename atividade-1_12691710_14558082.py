# Thiago Prado Dalla Dea  NUSP: 12691710
# André Dalla Dea Trombini  NUSP: 14558082


def ehPrimo(num, div=2):
    if num <= 1:
        return False
    if num == 2:
        return True
    if num % div == 0:
        return False
    if div * div > num:
        return True
    return ehPrimo(num, div + 1)


def numerosPrimos(x, y, listaPrimos=[]):
    if x > y:
        return listaPrimos
    if ehPrimo(x):
        listaPrimos.append(x)
    return numerosPrimos(x + 1, y, listaPrimos)


def maiorDiferenca(listaPrimos, posicao=0):
    if posicao == len(listaPrimos) - 1:
        return 0
    diferencaCabeca = listaPrimos[posicao + 1] - listaPrimos[posicao]
    diferencaCalda = maiorDiferenca(listaPrimos, posicao + 1)
    if diferencaCalda == 0 or diferencaCabeca > diferencaCalda:
        return diferencaCabeca
    return diferencaCalda


x = int(input())
y = int(input())
listaPrimos = numerosPrimos(x, y)
resultado = maiorDiferenca(listaPrimos)
print(resultado)
