import re
from collections import defaultdict
import json

def parse_and_partition():
    with open('clippy_errors.txt', 'r') as f:
        log = f.read()
    
    ansi_escape = re.compile(r'\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])')
    log = ansi_escape.sub('', log)
    
    pattern = re.compile(r'error:\s*(.*?)\n\s*-->\s*(crates/ggen-core/src/.*?):(\d+):')
    matches = pattern.findall(log)
    
    file_errors = defaultdict(list)
    for error_msg, filepath, line in matches:
        file_errors[filepath].append((int(line), error_msg))
        
    sorted_files = sorted(file_errors.items(), key=lambda x: len(x[1]), reverse=True)
    
    buckets = [[] for _ in range(10)]
    bucket_weights = [0] * 10
    
    for filepath, errs in sorted_files:
        min_idx = bucket_weights.index(min(bucket_weights))
        buckets[min_idx].append((filepath, errs))
        bucket_weights[min_idx] += len(errs)
        
    for i, bucket in enumerate(buckets):
        files = [filepath for filepath, _ in bucket]
        print(f"Bucket {i+1} files: {json.dumps(files)}")

if __name__ == '__main__':
    parse_and_partition()
