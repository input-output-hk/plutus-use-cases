import './src/Main.purs';

function importAll(resolve) {
    resolve.keys().forEach(resolve);
}

importAll(
    require.context('./src', true, /\.scss$/)
);
