export const read_as_array_buffer = (file) => {
    const reader = new FileReader();
    return new Promise((resolve, reject) => {
        reader.onerror = () => {
            reader.abort();
            reject(new DOMException("Problem parsing input file."));
        };

        reader.onload = () => {
            resolve(new Uint8Array(reader.result));
        };
        reader.readAsArrayBuffer(file);
    });
};
