import csv

sentences_file = "/opt/Downloads/lang_ru/sentences.csv"
links_file = "/opt/Downloads/lang_ru/links.csv"
output_file = "/opt/Downloads/lang_ru/en_ru_pairs.csv"

english_sentences = {}
russian_sentences = {}

print("Loading sentences...")
with open(sentences_file, "r", encoding="utf-8") as f:
    reader = csv.reader(f, delimiter="\t")
    for row in reader:
        if len(row) >= 3:
            s_id, lang, text = row[0], row[1], row[2]
            if lang == "eng":
                english_sentences[s_id] = text
            elif lang == "rus":
                russian_sentences[s_id] = text

print("Matching unique pairs...")

unique_pairs = []
seen_english = set()  # Track used English sentences

with open(links_file, "r", encoding="utf-8") as f:
    reader = csv.reader(f, delimiter="\t")
    for row in reader:
        if len(row) >= 2:
            id1, id2 = row[0], row[1]
            
            # Find English sentence text
            eng_text = None
            rus_text = None

            if id1 in english_sentences and id2 in russian_sentences:
                eng_text = english_sentences[id1]
                rus_text = russian_sentences[id2]
            elif id1 in russian_sentences and id2 in english_sentences:
                eng_text = english_sentences[id2]
                rus_text = russian_sentences[id1]

            # Only take the FIRST Russian translation for each English sentence
            if eng_text and rus_text:
                if eng_text not in seen_english:
                    seen_english.add(eng_text)
                    unique_pairs.append((rus_text, eng_text))

# Save unique pairs
with open(output_file, "w", encoding="utf-8", newline="") as f:
    writer = csv.writer(f)
    writer.writerow(["Russian", "English"])
    writer.writerows(unique_pairs)

print(f"Saved {len(unique_pairs)} unique sentence pairs to {output_file}!")
