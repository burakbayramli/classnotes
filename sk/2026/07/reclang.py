import asyncio
import csv
import os
import random
import edge_tts
from pydub import AudioSegment

# Configuration
CSV_FILE = "/opt/Downloads/lang_ru/en_ru.csv"
OUTPUT_MP3 = "/tmp/learning_track_randomized.mp3"

# Voice Settings
VOICE_EN = "en-US-ChristopherNeural"
VOICE_RU = "ru-RU-DmitryNeural"

# Silence Durations (in milliseconds)
PAUSE_SHORT = AudioSegment.silent(duration=1000)  # 1.0 sec between repeats/languages
PAUSE_LONG = AudioSegment.silent(duration=2500)   # 2.5 sec before next pair

async def generate_audio_clip(text, voice, rate="+0%"):
    """Helper to generate TTS audio and return it as a pydub AudioSegment."""
    temp_filename = f"temp_{abs(hash(text + voice + rate))}.mp3"
    communicate = edge_tts.Communicate(text, voice, rate=rate)
    await communicate.save(temp_filename)
    
    # Load into pydub and remove temporary file
    audio = AudioSegment.from_mp3(temp_filename)
    if os.path.exists(temp_filename):
        os.remove(temp_filename)
        
    return audio

async def build_continuous_track():
    # 1. Read all rows from CSV
    sentence_pairs = []
    with open(CSV_FILE, "r", encoding="utf-8") as f:
        reader = csv.reader(f)
        for row in reader:
            if row and len(row) >= 2:
                ru_text, en_text = row[0].strip(), row[1].strip()
                if ru_text and en_text:
                    sentence_pairs.append((ru_text, en_text))

    # 2. Shuffle sentence order randomly
    random.shuffle(sentence_pairs)
    print(f"Loaded {len(sentence_pairs)} pairs and randomized order.\n")

    combined_audio = AudioSegment.empty()

    # 3. Process each pair in random sequence
    for idx, (ru_text, en_text) in enumerate(sentence_pairs, 1):
        print(f"[{idx}/{len(sentence_pairs)}] EN: {en_text}")
        print(f"         RU: {ru_text}")

        # English (Normal)
        en_audio = await generate_audio_clip(en_text, VOICE_EN, rate="+0%")
        
        # Russian 1st reading (-20% speed)
        ru_audio_slow = await generate_audio_clip(ru_text, VOICE_RU, rate="-25%")
        
        # Russian 2nd reading (-35% speed)
        ru_audio_extra_slow = await generate_audio_clip(ru_text, VOICE_RU, rate="-35%")

        # Sequence: EN -> Pause -> RU (Slow) -> Pause -> RU (Extra Slow) -> Long Pause
        combined_audio += en_audio
        combined_audio += PAUSE_SHORT
        combined_audio += ru_audio_slow
        combined_audio += PAUSE_SHORT
        combined_audio += ru_audio_extra_slow
        combined_audio += PAUSE_LONG

    # 4. Export single output MP3
    combined_audio.export(OUTPUT_MP3, format="mp3")
    print(f"\nDone! Exported track to {OUTPUT_MP3}")

if __name__ == "__main__":
    asyncio.run(build_continuous_track())
