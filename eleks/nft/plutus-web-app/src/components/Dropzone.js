import Dropzone from 'react-dropzone';
import '../styles/Dropzone.scss';

const DropzoneComponent = ({ image, setImage, error }) => (
  <Dropzone
    onDrop={(images) =>
      setImage(
        Object.assign(images[0], {
          preview: URL.createObjectURL(images[0]),
        })
      )
    }
  >
    {({ getRootProps, getInputProps }) => (
      <section>
        <div {...getRootProps()} className={`Dropzone ${error && !image && 'error'}`}>
          <input {...getInputProps()} />
          {!image && (
            <p className='placeholder'>
              Drag 'n' drop some files here, or click to select files
            </p>
          )}
          {image && <img className='image' src={image.preview} />}
        </div>
      </section>
    )}
  </Dropzone>
);

export default DropzoneComponent;
