
wartosc_przyszla=function(kapital,stopa,lata){
  FV=kapital*(1+stopa)^lata
  return(FV)
}
print(wartosc_przyszla(5000,0.05,1))

przelicz_walute=function(kwota, kurs=4.32){
  W=kwota*kurs
  return(W)
}
print(przelicz_walute(100))
print(przelicz_walute(100,4.5))

kostka <- function(n) {
  sample(1:6, size = n, replace = TRUE)
}

kostka1=kostka(100)
table(kostka1)


kalkulator=function(a, b, operacja) {
  
  if (operacja == "+") {
    return(a + b)  
  } else if (operacja == "-") {
    return(a - b)
  } else if (operacja == "*") {
    return(a * b)   
  } else if (operacja == "/") {  
    if (b == 0) {
      return("Błąd: dzielenie przez zero!")
    } else {
      return(a / b)
    }
    
  } else {
    return("Nieznana operacja")
  }
}

kalkulator(20, 2, "+")
kalkulator(20, 2, "-")
kalkulator(20, 2, "*")
kalkulator(20, 2, "/")


przyznaj_nagrode=function() {
  rzut=sample(1:6, size = 1, replace = TRUE)
  if (rzut == 6) {
    return("Super bonus!")  
  } else if (rzut == 4 || rzut == 5) {
    return("Nagroda standardowa")
  } else {
    return("Brak nagrody")
  }
}

przyznaj_nagrode()
przyznaj_nagrode()


ocena_kredytowa=function(dochod, zadluzenie) {
  procent_zadluzenia=zadluzenie / dochod
  
  if (procent_zadluzenia < 0.30) {
    return("KREDYT PRZYZNANY")
  } else if (procent_zadluzenia <= 0.50) {
    return("WYMAGA WERYFIKACJI")
  } else {
    return("KREDYT ODRZUCONY")
  }
}


ocena_kredytowa(10000, 2000)
ocena_kredytowa(10000, 4000)


podatek_Belki=function(przychod, koszt, typ_aktywa) {
  
  zysk=przychod - koszt
  
  if (zysk <= 0) {
    return(0)
  }
  if (typ_aktywa == "akcje" || typ_aktywa == "obligacje") {
    return(0.19 * zysk)
  }
  if (typ_aktywa == "kryptowaluty") {
    
    prog=85528
    
    if (zysk <= prog) {
      return(0.18 * zysk)
    } else {
      podatek <- (0.18 * prog) + (0.32 * (zysk - prog))
      return(podatek)
    }
  }
  return("Nieznany typ aktywa")
}

podatek_Belki(15000, 10000, "akcje")        
podatek_Belki(8000, 10000, "akcje")         
podatek_Belki(150000, 50000, "kryptowaluty")
podatek_Belki(1500, 1000, "lokata")


typ_gospodarstwa=function(dochod, wydatki, dzieci, miasto) {

  if (wydatki > dochod) {
    return("Trudna sytuacja")
  }

  if (wydatki <= 0.8 * dochod && miasto == "male") {
    return("Stabilna sytuacja")
  }

  if (wydatki <= dochod && dzieci >= 2) {
    return("Przeciętna sytuacja")
  }

  return("Przeciętna sytuacja")
}


typ_gospodarstwa(4000, 4500, 1, "duze")
typ_gospodarstwa(5000, 4800, 2, "duze")
typ_gospodarstwa(5000, 3500, 0, "male")











