// Pure Vanilla JavaScript - WASM Crypto Demo
// No frameworks, no dependencies, just WASM and JS

let wasmModule = null;
let currentKey = null;
let currentEncrypted = null;
let currentKeypair = null;
let currentSignature = null;
let performanceMetrics = {
    hashTime: 0,
    encryptTime: 0,
    signTime: 0,
    totalOps: 0
};

// Initialize WASM module
async function init() {
    try {
        // Import the WASM module
        const module = await import('./pkg/wasm_crypto.js');
        await module.default();
        wasmModule = module;

        // Get module info
        const info = wasmModule.get_info();
        displayModuleInfo(info);

        // Generate initial key
        currentKey = wasmModule.generate_key();

        // Hide loading, show app
        document.getElementById('loading').classList.add('hidden');
        document.getElementById('app').classList.remove('hidden');

        // Update stats
        updateStats();

        console.log('✅ WASM Crypto module loaded successfully');
    } catch (error) {
        document.getElementById('loading').innerHTML = `
            <div style="color: #f44336;">❌ Error loading WASM module</div>
            <div style="margin-top: 10px; font-size: 0.9em;">${error.message}</div>
            <div style="margin-top: 10px; font-size: 0.85em;">Make sure to run: wasm-pack build --target web</div>
        `;
        console.error('WASM initialization error:', error);
    }
}

function displayModuleInfo(info) {
    const capabilities = info.capabilities.map(cap =>
        `<span class="capability-badge">${cap}</span>`
    ).join('');

    document.getElementById('module-info').innerHTML = `
        <div><strong>Name:</strong> ${info.name} v${info.version}</div>
        <div class="capabilities">${capabilities}</div>
    `;
}

function updateStats() {
    const avgHashTime = performanceMetrics.hashTime.toFixed(2);
    const avgEncryptTime = performanceMetrics.encryptTime.toFixed(2);
    const avgSignTime = performanceMetrics.signTime.toFixed(2);

    document.getElementById('stats').innerHTML = `
        <div class="stat-card">
            <h3>Total Operations</h3>
            <div class="value">${performanceMetrics.totalOps}</div>
        </div>
        <div class="stat-card">
            <h3>Avg Hash Time</h3>
            <div class="value">${avgHashTime}ms</div>
        </div>
        <div class="stat-card">
            <h3>Avg Encrypt Time</h3>
            <div class="value">${avgEncryptTime}ms</div>
        </div>
        <div class="stat-card">
            <h3>Avg Sign Time</h3>
            <div class="value">${avgSignTime}ms</div>
        </div>
    `;
}

function measurePerformance(fn, metricKey) {
    const start = performance.now();
    const result = fn();
    const end = performance.now();
    const duration = end - start;

    performanceMetrics[metricKey] = duration;
    performanceMetrics.totalOps++;
    updateStats();

    return { result, duration };
}

function showOutput(elementId, content, type = 'success') {
    const element = document.getElementById(elementId);
    element.className = `output ${type}`;
    element.innerHTML = content;
}

// Password Hashing Functions
let currentHash = null;

window.hashPassword = function() {
    const password = document.getElementById('password-input').value;

    if (!password) {
        showOutput('password-output', '❌ Please enter a password', 'error');
        return;
    }

    const { result, duration } = measurePerformance(() => {
        return wasmModule.hash_password(password, null);
    }, 'hashTime');

    currentHash = result;

    showOutput('password-output', `
        ✅ Password hashed successfully
        <span class="perf-indicator fast">⚡ ${duration.toFixed(2)}ms</span>
        <br><br>
        <strong>Hash:</strong><br>${result}
        <br><br>
        <em>Try clicking "Verify Password" to test verification</em>
    `);
};

window.verifyPassword = function() {
    const password = document.getElementById('password-input').value;

    if (!currentHash) {
        showOutput('password-output', '❌ Please hash a password first', 'error');
        return;
    }

    const { result, duration } = measurePerformance(() => {
        return wasmModule.verify_password(password, currentHash);
    }, 'hashTime');

    if (result) {
        showOutput('password-output', `
            ✅ Password verified successfully
            <span class="perf-indicator fast">⚡ ${duration.toFixed(2)}ms</span>
            <br><br>
            The password matches the hash!
        `);
    } else {
        showOutput('password-output', `
            ❌ Password verification failed
            <span class="perf-indicator fast">⚡ ${duration.toFixed(2)}ms</span>
            <br><br>
            The password does not match the hash.
        `, 'error');
    }
};

// Encryption Functions
window.generateNewKey = function() {
    currentKey = wasmModule.generate_key();
    showOutput('encryption-output', `
        ✅ New 256-bit encryption key generated
        <br><br>
        <strong>Key (base64):</strong><br>${arrayToBase64(currentKey)}
    `);
};

window.performEncryption = function() {
    const plaintext = document.getElementById('encrypt-input').value;

    if (!plaintext) {
        showOutput('encryption-output', '❌ Please enter text to encrypt', 'error');
        return;
    }

    const { result, duration } = measurePerformance(() => {
        const encoder = new TextEncoder();
        const data = encoder.encode(plaintext);
        return wasmModule.encrypt_aes(data, currentKey, null);
    }, 'encryptTime');

    currentEncrypted = result;

    showOutput('encryption-output', `
        ✅ Data encrypted successfully
        <span class="perf-indicator fast">⚡ ${duration.toFixed(2)}ms</span>
        <br><br>
        <strong>Ciphertext:</strong><br>${result.ciphertext}
        <br><br>
        <strong>Nonce:</strong><br>${result.nonce}
        <br><br>
        <em>Try clicking "Decrypt" to recover the original text</em>
    `);
};

window.performDecryption = function() {
    if (!currentEncrypted) {
        showOutput('encryption-output', '❌ Please encrypt some data first', 'error');
        return;
    }

    const { result, duration } = measurePerformance(() => {
        return wasmModule.decrypt_aes(
            currentEncrypted.ciphertext,
            currentKey,
            currentEncrypted.nonce
        );
    }, 'encryptTime');

    const decoder = new TextDecoder();
    const plaintext = decoder.decode(new Uint8Array(result));

    showOutput('encryption-output', `
        ✅ Data decrypted successfully
        <span class="perf-indicator fast">⚡ ${duration.toFixed(2)}ms</span>
        <br><br>
        <strong>Plaintext:</strong><br>${plaintext}
    `);
};

// Signature Functions
window.generateSignatureKeypair = function() {
    const { result, duration } = measurePerformance(() => {
        return wasmModule.generate_keypair();
    }, 'signTime');

    currentKeypair = result;

    showOutput('signature-output', `
        ✅ Ed25519 keypair generated
        <span class="perf-indicator fast">⚡ ${duration.toFixed(2)}ms</span>
        <br><br>
        <strong>Public Key:</strong><br>${result.publicKey}
        <br><br>
        <strong>Secret Key:</strong><br>${result.secretKey.substring(0, 32)}...
        <br><br>
        <em>Try clicking "Sign Message" to create a signature</em>
    `);
};

window.signMessage = function() {
    const message = document.getElementById('signature-input').value;

    if (!currentKeypair) {
        showOutput('signature-output', '❌ Please generate a keypair first', 'error');
        return;
    }

    const { result, duration } = measurePerformance(() => {
        const encoder = new TextEncoder();
        const data = encoder.encode(message);
        return wasmModule.sign_message(data, currentKeypair.secretKey);
    }, 'signTime');

    currentSignature = result;

    showOutput('signature-output', `
        ✅ Message signed successfully
        <span class="perf-indicator fast">⚡ ${duration.toFixed(2)}ms</span>
        <br><br>
        <strong>Signature:</strong><br>${result}
        <br><br>
        <em>Try clicking "Verify Signature" to verify authenticity</em>
    `);
};

window.verifySignature = function() {
    const message = document.getElementById('signature-input').value;

    if (!currentSignature || !currentKeypair) {
        showOutput('signature-output', '❌ Please sign a message first', 'error');
        return;
    }

    const { result, duration } = measurePerformance(() => {
        const encoder = new TextEncoder();
        const data = encoder.encode(message);
        return wasmModule.verify_signature(data, currentSignature, currentKeypair.publicKey);
    }, 'signTime');

    if (result) {
        showOutput('signature-output', `
            ✅ Signature verified successfully
            <span class="perf-indicator fast">⚡ ${duration.toFixed(2)}ms</span>
            <br><br>
            The signature is valid and matches the message!
        `);
    } else {
        showOutput('signature-output', `
            ❌ Signature verification failed
            <span class="perf-indicator fast">⚡ ${duration.toFixed(2)}ms</span>
            <br><br>
            The signature does not match the message.
        `, 'error');
    }
};

// Hash Functions
window.performHash = function() {
    const data = document.getElementById('hash-input').value;

    if (!data) {
        showOutput('hash-output', '❌ Please enter data to hash', 'error');
        return;
    }

    const { result, duration } = measurePerformance(() => {
        const encoder = new TextEncoder();
        const bytes = encoder.encode(data);
        return wasmModule.hash_sha256(bytes);
    }, 'hashTime');

    showOutput('hash-output', `
        ✅ SHA-256 hash computed
        <span class="perf-indicator fast">⚡ ${duration.toFixed(2)}ms</span>
        <br><br>
        <strong>Hash:</strong><br>${result}
    `);
};

window.generateRandom = function() {
    const { result, duration } = measurePerformance(() => {
        return wasmModule.random_bytes(32);
    }, 'hashTime');

    showOutput('hash-output', `
        ✅ Random bytes generated
        <span class="perf-indicator fast">⚡ ${duration.toFixed(2)}ms</span>
        <br><br>
        <strong>Random (base64):</strong><br>${arrayToBase64(result)}
        <br><br>
        <strong>Random (hex):</strong><br>${arrayToHex(result)}
    `);
};

// Utility functions
function arrayToBase64(array) {
    return btoa(String.fromCharCode.apply(null, array));
}

function arrayToHex(array) {
    return Array.from(array)
        .map(b => b.toString(16).padStart(2, '0'))
        .join('');
}

// Initialize on page load
init();
