# Miladi, Hicri, Ay, Güneş Takvimleri

Miladi ve Hicri takvimleri arasında çevirim yapabilmek için alttaki kodlar,
`node` ortamında işletmek için `dateutil.js` içinde

```javascript

module.exports = { hijriToJulian: hijriToJulian,
		   gregorianToJulian: gregorianToJulian,
		   julianToGregorian: julianToGregorian,
		   julianToHijri: julianToHijri}

function hijriToJulian(year, month, day){
  return (
    Math.floor((11 * year + 3) / 30) +
    Math.floor(354 * year) +
    Math.floor(30 * month) -
    Math.floor((month - 1) / 2) +
    day +
    1948440 -
    386
  );
}

function gregorianToJulian (year, month, day) {
  if (month < 3) {
    year -= 1;
    month += 12;
  }

  const a = Math.floor(year / 100.0);
  const b = year === 1582 && (month > 10 || (month === 10 && day > 4))
      ? -10 :
      year === 1582 && month === 10
      ? 0 :
      year < 1583
      ? 0 :
      2 - a + Math.floor(a / 4.0);

  return Math.floor(365.25 * (year + 4716)) + Math.floor(30.6001 * (month + 1)) + day + b - 1524;
}

function julianToHijri (julianDay) {
  const y = 10631.0 / 30.0;
  const epochAstro = 1948084;
  const shift1 = 8.01 / 60.0;

  let z = julianDay - epochAstro;
  const cyc = Math.floor(z / 10631.0);
  z -= 10631 * cyc;
  const j = Math.floor((z - shift1) / y);
  z -= Math.floor(j * y + shift1);

  const year = 30 * cyc + j;
  let month = Math.floor(parseInt((z + 28.5001) / 29.5));
  if (month === 13) {
    month = 12;
  }

  const day = z - Math.floor(29.5001 * month - 29);

  return { year: parseInt(year), month: parseInt(month), day: parseInt(day) };
}

function julianToGregorian (julianDate) {
  let b = 0;
  if (julianDate > 2299160) {
    const a = Math.floor((julianDate - 1867216.25) / 36524.25);
    b = 1 + a - Math.floor(a / 4.0);
  }

  const bb = julianDate + b + 1524;
  let cc = Math.floor((bb - 122.1) / 365.25);
  const dd = Math.floor(365.25 * cc);
  const ee = Math.floor((bb - dd) / 30.6001);

  const day = bb - dd - Math.floor(30.6001 * ee);
  let month = ee - 1;

  if (ee > 13) {
    cc += 1;
    month = ee - 13;
  }

  const year = cc - 4716;

  return { year: parseInt(year), month: parseInt(month), day: parseInt(day) };
}
```

Bu kodu kullanabilmek için kurban bayramı ilk günü hesabı
yapalım. Bayramın hicri takviminde yılın son ayın 10'uncu günü
olduğunu biliyoruz, 2024 senesinde bu Temmuz'un 16'sı idi,


```javascript
var sh = require('./dateutil.js')

// Eid Al-Adha (kurban) was at this day 2024
const y = 2024;
const m = 6;
const d = 16;

const julianDay = sh.gregorianToJulian(y, m, d);

const { year, month, day } = sh.julianToHijri(julianDay);

console.log(year, month, day);
```

```
1445 12 10
```

Doğru ay ve gün verildi, sene de öyle. Peki bir sonraki senede 1446 (2025)
yılında bayram hangi güne denk gelir?


```javascript
// Eid Al-Adha (kurban) is 10th day of last month
var res = sh.hijriToJulian(1446, 12, 10);
res = sh.julianToGregorian(res);
console.log(res.year, res.month, res.day);
```

```
2025 6 6
```
