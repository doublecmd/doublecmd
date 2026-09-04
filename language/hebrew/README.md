# תרגום עברי ל-Double Commander

התרגום העברי מורכב משני קבצים:

| קובץ | תיאור |
|---|---|
| `../doublecmd.he.po` | תרגום ראשי של התוכנית (כל המחרוזות של Double Commander) |
| `../lcl/lclstrconsts.he.po` | תרגום של רכיבי LCL (Lazarus) — חלקו כבר היה קיים והושלם |

הקבצים נוצרים/מתעדכנים על ידי סקריפטים של Python שנמצאים בתיקייה זו (`language/hebrew/`).

---

## איך מעדכנים את התרגום

### דרישות
- Python 3 (בכל פלטפורמה)

### שלב 1: עדכון הקובץ הראשי (`doublecmd.he.po`)

התרגום נשמר כמילון של Python (מקור האמת), וממנו נוצר קובץ ה-.po:

```
he_dict_01.py  ← מחרוזות 1–150
he_dict_02.py  ← מחרוזות 151–800
he_dict_03.py  ← מחרוזות 801–3258
he_dict_04.py  ← מחרוזות נוספות (רב-שורתיות)
he_dict.py     ← מאחד את כל החלקים
```

כדי ליצור מחדש את `doublecmd.he.po` מהתבנית `doublecmd.pot`:

```bash
# מתוך שורש ה-repository
python language/hebrew/gen_he.py
```

הסקריפט:
1. קורא את `language/doublecmd.pot` (התבנית המעודכנת).
2. מצמיד לכל `msgid` את התרגום מהמילון.
3. כותב את `language/doublecmd.he.po`.

אם יש מחרוזות חדשות שעדיין לא תורגמו, הסקריפט ידפיס אותן כ-`MISSING:` בסוף הריצה — יש להוסיף אותן למילון המתאים ואז להריץ שוב.

### שלב 2: עדכון קובץ LCL (`lclstrconsts.he.po`)

תרגומי ה-LCL נשמרים במילון `he_dict_lcl.py`, והסקריפט ממלא רק מחרוזות **ריקות** בקובץ הקיים (לא נוגע בתרגומים שכבר קיימים):

```bash
python language/hebrew/gen_lcl.py
```

### שלב 3: אימות

```bash
# בדיקת מבנה (msgid/msgstr תקינים, ללא תרגומים ריקים)
python language/hebrew/validate_po.py ../doublecmd.he.po ../lcl/lclstrconsts.he.po

# בדיקת שמירת placeholders (%d, %s, %f...)
python language/hebrew/check_placeholders.py ../doublecmd.he.po ../lcl/lclstrconsts.he.po
```

(שני סקריפטי האימות נמצאים ב-`/tmp` במהלך הפיתוח — ניתן להעתיק אותם לתיקייה זו במידת הצורך.)

---

## כללי תרגום חשובים

1. **Placeholders** — יש לשמור את כל הסימונים כמו `%d`, `%s`, `%f` בדיוק כפי שהם (ובאותו סדר).
2. **מקשי קיצור** — הסימן `&` מסמן מקש קיצור (accelerator). בעברית נהוג להשאיר את `&` לפני אות עברית, למשל `&שמירה`. הסימן `&&` מציין `&` פשוט וצריך להישאר.
3. **שמות טכניים** — שמות כמו "Double Commander", "Free Pascal", "Lazarus", `URL`, `FTP` וכדומה נשארים באנגלית.
4. **סגנון** — התרגום בסגנון רשמי (פניה ב"אתה").

---

## איך לראות את התרגום

1. העתק את `language/doublecmd.he.po` לתיקיית ה-`language/` של ההתקנה.
2. העתק את `language/lcl/lclstrconsts.he.po` לתיקיית ה-`language/lcl/` של ההתקנה.
3. הפעל את Double Commander → **אפשרויות (Options) → שפה (Language)** → בחר **Hebrew / עברית**.

> הערה: תיקיית ה-language נסרקת אוטומטית, כך שאין צורך לשנות קוד או לבנות מחדש.

---

## שינויי תרגום מול ה-PR

כאשר מגישים PR עם תרגום עברי, יש לכלול:
- `language/doublecmd.he.po` (תרגום חדש)
- `language/lcl/lclstrconsts.he.po` (השלמת תרגום)
- תיקיית `language/hebrew/` (המילונים והסקריפטים לעדכון עתידי)
